// Package tui is the Bubble Tea wizard that drives the whole installer: it
// collects the user's choices (version, features, packages, daemon), then runs
// the compile/install pipeline with a live progress view.
package tui

import (
	"context"
	"os"
	"os/exec"
	"strconv"
	"strings"

	"github.com/charmbracelet/bubbles/progress"
	"github.com/charmbracelet/bubbles/spinner"
	"github.com/charmbracelet/bubbles/viewport"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"

	"github.com/Borderliner/emacs-installer/internal/emacs"
	"github.com/Borderliner/emacs-installer/internal/pkg"
	"github.com/Borderliner/emacs-installer/internal/sys"
)

type step int

const (
	stepWelcome step = iota
	stepVersion
	stepFlags
	stepDeps
	stepService
	stepReview
	stepInstall
	stepUninstall
	stepDone
	stepError
)

var crumbs = []string{"Version", "Features", "Packages", "Daemon", "Review", "Install"}

// crumbIndex maps a wizard step to its breadcrumb position (-1 = none).
func crumbIndex(s step) int {
	switch s {
	case stepVersion:
		return 0
	case stepFlags:
		return 1
	case stepDeps:
		return 2
	case stepService:
		return 3
	case stepReview:
		return 4
	case stepInstall, stepDone:
		return 5
	default:
		return -1
	}
}

// Options configures a run of the wizard.
type Options struct {
	DryRun    bool
	Su        string // preferred escalation tool ("sudo"/"doas"/"")
	Prefix    string
	Uninstall bool // start in uninstall/cleanup mode
}

// Model is the whole wizard state.
type Model struct {
	opts   Options
	prefix string
	dryRun bool

	width, height int

	info sys.Info
	env  emacs.Env
	ctx  context.Context
	stop context.CancelFunc

	spinner  spinner.Model
	progress progress.Model
	logView  viewport.Model

	step step

	// version
	versions   []emacs.Version
	verLoading bool
	verErr     error
	verCursor  int

	// features
	toolkits   []emacs.Toolkit
	toggles    []emacs.Toggle
	sel        emacs.Selection
	flagCursor int

	// dependency resolution
	pkgManager    *pkg.Manager
	configureArgs []string
	logicalDeps   []string
	resolving     bool
	resolution    pkg.Resolution
	resolveErr    error

	// daemon / integration
	daemon        bool
	symlink       bool
	desktop       bool
	inits         []sys.InitSystem
	initCursor    int
	serviceCursor int

	// install run
	cfg        emacs.Config
	phases     []emacs.Phase
	phaseIdx   int
	curFrac    float64
	logLines   []string
	evCh       chan emacs.Progress
	errCh      chan error
	installErr error

	// uninstall mode
	uninstall       bool
	manifest        emacs.Manifest
	manifestOK      bool
	deleteSource    bool
	existingInstall bool

	quitting bool
}

// New builds the initial model and detects the host.
func New(opts Options) Model {
	if opts.Prefix == "" {
		opts.Prefix = "/opt/emacs"
	}
	info := sys.Detect(opts.Su)

	sp := spinner.New()
	sp.Spinner = spinner.Points
	sp.Style = lipgloss.NewStyle().Foreground(cAccent)

	pr := progress.New(progress.WithGradient("#CBA6F7", "#F5C2E7"), progress.WithoutPercentage())

	ctx, stop := context.WithCancel(context.Background())

	inits := sys.AvailableInits()
	if len(inits) == 0 {
		inits = []sys.InitSystem{info.Init}
	}
	initCursor := 0
	for i, in := range inits {
		if in == info.Init {
			initCursor = i
		}
	}

	m := Model{
		opts:       opts,
		prefix:     opts.Prefix,
		dryRun:     opts.DryRun,
		info:       info,
		ctx:        ctx,
		stop:       stop,
		spinner:    sp,
		progress:   pr,
		logView:    viewport.New(0, 0),
		step:       stepWelcome,
		verLoading: true,
		daemon:     true,
		symlink:    true,
		desktop:    info.OS != "darwin",
		inits:      inits,
		initCursor: initCursor,
	}

	if mf, ok := emacs.LoadManifest(info.Home); ok {
		m.existingInstall = emacs.InstallPresent(mf)
	} else if _, err := os.Stat(opts.Prefix); err == nil {
		m.existingInstall = true
	}

	if opts.Uninstall {
		m.uninstall = true
		m.verLoading = false
		mf, ok := emacs.LoadManifest(info.Home)
		if !ok {
			mf = emacs.DeriveManifest(info, opts.Prefix)
		}
		m.manifest = mf
		m.manifestOK = ok
		m.step = stepUninstall
	}
	return m
}

func (m Model) Init() tea.Cmd {
	if m.uninstall {
		return m.spinner.Tick
	}
	return tea.Batch(m.spinner.Tick, loadVersions())
}

// --- messages & commands ----------------------------------------------------

type versionsLoadedMsg struct {
	versions []emacs.Version
	err      error
}
type depsResolvedMsg struct {
	res pkg.Resolution
	mgr *pkg.Manager
	err error
}
type progressMsg emacs.Progress
type phaseDoneMsg struct{ err error }

func loadVersions() tea.Cmd {
	return func() tea.Msg {
		vs, err := emacs.Discover(context.Background())
		return versionsLoadedMsg{versions: vs, err: err}
	}
}

func resolveDeps(mgr *pkg.Manager, fam sys.Family, deps []string) tea.Cmd {
	return func() tea.Msg {
		if mgr == nil {
			return depsResolvedMsg{mgr: nil}
		}
		return depsResolvedMsg{res: pkg.Resolve(mgr, fam, deps), mgr: mgr}
	}
}

func waitPhase(ch chan emacs.Progress, errCh chan error) tea.Cmd {
	return func() tea.Msg {
		if p, ok := <-ch; ok {
			return progressMsg(p)
		}
		return phaseDoneMsg{err: <-errCh}
	}
}

// --- update -----------------------------------------------------------------

func (m Model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.width, m.height = msg.Width, msg.Height
		m.relayout()
		return m, nil

	case tea.KeyMsg:
		switch msg.String() {
		case "ctrl+c":
			m.stop()
			m.quitting = true
			return m, tea.Quit
		case "q":
			if m.step != stepInstall {
				m.stop()
				m.quitting = true
				return m, tea.Quit
			}
		}
		return m.handleKey(msg)

	case tea.MouseMsg:
		if m.step == stepInstall {
			var cmd tea.Cmd
			m.logView, cmd = m.logView.Update(msg)
			return m, cmd
		}
		return m, nil

	case spinner.TickMsg:
		var cmd tea.Cmd
		m.spinner, cmd = m.spinner.Update(msg)
		return m, cmd

	case versionsLoadedMsg:
		m.verLoading = false
		m.verErr = msg.err
		if len(msg.versions) > 0 {
			m.versions = msg.versions
		} else {
			m.versions = emacs.Fallback()
		}
		return m, nil

	case depsResolvedMsg:
		m.resolving = false
		m.resolveErr = msg.err
		m.pkgManager = msg.mgr
		m.resolution = msg.res
		return m, nil

	case progressMsg:
		p := emacs.Progress(msg)
		if p.Line != "" {
			m.appendLog(p.Line)
		}
		if p.Fraction >= 0 {
			m.curFrac = p.Fraction
		}
		return m, waitPhase(m.evCh, m.errCh)

	case phaseDoneMsg:
		return m.phaseDone(msg.err)
	}
	return m, nil
}

func (m Model) handleKey(msg tea.KeyMsg) (tea.Model, tea.Cmd) {
	// Global back navigation.
	if msg.String() == "esc" {
		switch m.step {
		case stepVersion:
			m.step = stepWelcome
		case stepFlags:
			m.step = stepVersion
		case stepDeps:
			m.step = stepFlags
		case stepService:
			m.step = stepDeps
		case stepReview:
			m.step = stepService
		}
		return m, nil
	}

	switch m.step {
	case stepWelcome:
		return m.welcomeKey(msg)
	case stepVersion:
		return m.versionKey(msg)
	case stepFlags:
		return m.flagsKey(msg)
	case stepDeps:
		return m.depsKey(msg)
	case stepService:
		return m.serviceKey(msg)
	case stepReview:
		return m.reviewKey(msg)
	case stepUninstall:
		return m.uninstallKey(msg)
	case stepInstall:
		// Let the user scroll back through the build log; ignore everything
		// else (use ctrl+c to abort a running build).
		switch msg.String() {
		case "up", "down", "k", "j", "pgup", "pgdown", "home", "end":
			var cmd tea.Cmd
			m.logView, cmd = m.logView.Update(msg)
			return m, cmd
		}
		return m, nil
	case stepDone, stepError:
		m.quitting = true
		return m, tea.Quit
	}
	return m, nil
}

func (m Model) phaseDone(err error) (tea.Model, tea.Cmd) {
	if err != nil {
		m.installErr = err
		m.step = stepError
		return m, nil
	}
	if m.phaseIdx < len(m.phases) {
		m.appendLog(stGood.Render("✓ " + m.phases[m.phaseIdx].Title))
	}
	return m.startPhase(m.phaseIdx + 1)
}

// startPhase begins phase i (or finishes the run when i is past the end).
func (m Model) startPhase(i int) (tea.Model, tea.Cmd) {
	m.phaseIdx = i
	m.curFrac = 0
	if i >= len(m.phases) {
		m.step = stepDone
		return m, nil
	}
	ph := m.phases[i]
	m.appendLog("")
	m.appendLog(stHeading.Render("── " + ph.Title + " ──"))

	if ph.Privileged {
		return m, m.execPrivileged(ph.Script)
	}
	ch := make(chan emacs.Progress, 256)
	errCh := make(chan error, 1)
	m.evCh, m.errCh = ch, errCh
	go func() {
		errCh <- ph.Run(m.ctx, ch)
		close(ch)
	}()
	return m, waitPhase(ch, errCh)
}

// execPrivileged runs a root shell script by suspending the TUI so sudo/doas can
// prompt for a password on the real terminal, then resumes.
func (m Model) execPrivileged(script string) tea.Cmd {
	argv := m.info.Escalation.WrapShell(script)
	c := exec.Command(argv[0], argv[1:]...)
	return tea.ExecProcess(c, func(err error) tea.Msg { return phaseDoneMsg{err: err} })
}

// --- helpers ----------------------------------------------------------------

func (m *Model) appendLog(line string) {
	m.logLines = append(m.logLines, line)
	if len(m.logLines) > 1000 {
		m.logLines = m.logLines[len(m.logLines)-1000:]
	}
	m.logView.SetContent(strings.Join(m.logLines, "\n"))
	m.logView.GotoBottom()
}

func (m *Model) relayout() {
	inner := m.bodyWidth() // usable width inside the card's border + padding
	if inner < 24 {
		inner = 24
	}
	m.progress.Width = inner - 6 // leave room for the "  100%" suffix
	m.logView.Width = inner - 2  // leave room for the log box's own border
	h := m.height - 18
	if h < 6 {
		h = 6
	}
	m.logView.Height = h
}

func (m Model) contentWidth() int {
	w := m.width - 6
	if w > 100 {
		w = 100
	}
	if w < 48 {
		w = 48
	}
	return w
}

func (m Model) selVersion() emacs.Version {
	if m.verCursor >= 0 && m.verCursor < len(m.versions) {
		return m.versions[m.verCursor]
	}
	return emacs.Version{}
}

func (m Model) selectedInit() sys.InitSystem {
	if m.initCursor >= 0 && m.initCursor < len(m.inits) {
		return m.inits[m.initCursor]
	}
	return m.info.Init
}

func (m Model) overallFrac() float64 {
	if len(m.phases) == 0 {
		return 0
	}
	return clamp01((float64(m.phaseIdx) + clamp01(m.curFrac)) / float64(len(m.phases)))
}

func clamp01(f float64) float64 {
	if f < 0 {
		return 0
	}
	if f > 1 {
		return 1
	}
	return f
}

func majorOf(number string) int {
	if i := strings.IndexByte(number, '.'); i > 0 {
		n, _ := strconv.Atoi(number[:i])
		return n
	}
	n, _ := strconv.Atoi(number)
	return n
}

func isWayland() bool {
	return os.Getenv("WAYLAND_DISPLAY") != "" || strings.EqualFold(os.Getenv("XDG_SESSION_TYPE"), "wayland")
}

// Run launches the wizard.
func Run(opts Options) error {
	p := tea.NewProgram(New(opts), tea.WithAltScreen(), tea.WithMouseCellMotion())
	_, err := p.Run()
	return err
}
