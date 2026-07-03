package tui

import (
	"fmt"
	"strings"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"

	"github.com/hajianpour/emacs-installer/internal/emacs"
	"github.com/hajianpour/emacs-installer/internal/pkg"
	"github.com/hajianpour/emacs-installer/internal/service"
	"github.com/hajianpour/emacs-installer/internal/sys"
)

// --- shared layout ----------------------------------------------------------

func (m Model) View() string {
	if m.quitting {
		return ""
	}
	switch m.step {
	case stepWelcome:
		return m.welcomeView()
	case stepVersion:
		return m.versionView()
	case stepFlags:
		return m.flagsView()
	case stepDeps:
		return m.depsView()
	case stepService:
		return m.serviceView()
	case stepReview:
		return m.reviewView()
	case stepInstall:
		return m.installView()
	case stepDone:
		return m.doneView()
	case stepError:
		return m.errorView()
	}
	return ""
}

func (m Model) sysSummary() string {
	esc := m.info.Escalation.Label()
	return fmt.Sprintf("%s  ·  %s  ·  init: %s  ·  %d threads  ·  root: %s",
		m.info.Distro.Name, m.info.Distro.Family, m.info.Init.Title(), m.info.Cores, esc)
}

// frame composes the header, breadcrumb, a bordered card around body, and footer.
func (m Model) frame(body, footer string) string {
	cw := m.contentWidth()
	parts := []string{
		"",
		stTitle.Render("  Emacs Installer"),
		"  " + stSubtle.Render(m.sysSummary()),
		"  " + breadcrumb(crumbs, crumbIndex(m.step)),
		"",
		stCard.Width(cw).Render(body),
	}
	if footer != "" {
		parts = append(parts, "  "+footer)
	}
	return lipgloss.JoinVertical(lipgloss.Left, parts...)
}

func (m Model) bodyWidth() int { return m.contentWidth() - 8 }

func wrap(s string, w int) string {
	return lipgloss.NewStyle().Width(w).Render(s)
}

// --- welcome ----------------------------------------------------------------

func (m Model) welcomeKey(msg tea.KeyMsg) (tea.Model, tea.Cmd) {
	switch msg.String() {
	case "enter", " ", "space":
		m.step = stepVersion
	case "q":
		m.quitting = true
		return m, tea.Quit
	}
	return m, nil
}

func (m Model) welcomeView() string {
	var b strings.Builder
	b.WriteString(stHeading.Render("Compile & install GNU Emacs from source") + "\n\n")
	b.WriteString(stText.Render(wrap("This wizard downloads the Emacs source, installs the build dependencies for your distribution, compiles it with every CPU thread, and installs it into ", m.bodyWidth())))
	b.WriteString(stKey.Render(m.prefix) + ".\n\n")

	rows := [][2]string{
		{"Distribution", m.info.Distro.Name + "  (" + string(m.info.Distro.Family) + ")"},
		{"Init system", m.info.Init.Title()},
		{"Privileges", m.info.Escalation.Label()},
		{"CPU threads", fmt.Sprintf("%d", m.info.Cores)},
	}
	for _, r := range rows {
		b.WriteString("  " + stSubtle.Render(fmt.Sprintf("%-14s", r[0])) + " " + stText.Render(r[1]) + "\n")
	}

	if !m.info.Escalation.Available && !m.dryRun {
		b.WriteString("\n" + stWarn.Render("⚠ Neither sudo nor doas found and not running as root — installing to "+m.prefix+" will fail. Try --dry-run or run as root."))
	}
	if m.dryRun {
		b.WriteString("\n" + stInfo.Render("● Dry-run mode: commands are shown, nothing is changed."))
	}
	if m.verLoading {
		b.WriteString("\n\n" + m.spinner.View() + " " + stMuted.Render("discovering available versions…"))
	}

	footer := joinHints(keyHint("enter", "begin"), keyHint("q", "quit"))
	return m.frame(b.String(), footer)
}

// --- version ----------------------------------------------------------------

func (m Model) versionKey(msg tea.KeyMsg) (tea.Model, tea.Cmd) {
	switch msg.String() {
	case "up", "k":
		if m.verCursor > 0 {
			m.verCursor--
		}
	case "down", "j":
		if m.verCursor < len(m.versions)-1 {
			m.verCursor++
		}
	case "enter", " ", "space":
		if len(m.versions) == 0 {
			return m, nil
		}
		v := m.selVersion()
		m.env = emacs.Env{
			OS:         m.info.OS,
			Systemd:    m.info.Init == sys.InitSystemd,
			Wayland:    isWayland(),
			EmacsMajor: majorOf(v.Number),
		}
		m.toolkits = emacs.Toolkits(m.env)
		m.toggles = emacs.Toggles(m.env)
		m.sel = emacs.DefaultSelection(m.env)
		m.flagCursor = 0
		m.step = stepFlags
	}
	return m, nil
}

func (m Model) versionView() string {
	var b strings.Builder
	b.WriteString(stHeading.Render("Choose an Emacs version") + "\n")

	if m.verLoading {
		b.WriteString("\n" + m.spinner.View() + " " + stMuted.Render("querying the GNU mirrors…"))
		return m.frame(b.String(), joinHints(keyHint("esc", "back")))
	}
	if m.verErr != nil {
		b.WriteString(stWarn.Render("offline — showing the built-in fallback list") + "\n")
	}
	b.WriteString("\n")
	for i, v := range m.versions {
		focused := i == m.verCursor
		name := fmt.Sprintf("%-16s", v.Title())
		if focused {
			b.WriteString(cursor(true) + stSelected.Render(name) + "  " + stText.Render(v.Desc()) + "\n")
		} else {
			b.WriteString(cursor(false) + stText.Render(name) + "  " + stMuted.Render(v.Desc()) + "\n")
		}
	}

	footer := joinHints(keyHint("↑/↓", "move"), keyHint("enter", "select"), keyHint("esc", "back"))
	return m.frame(b.String(), footer)
}

// --- features (toolkit + toggles) -------------------------------------------

// flagItems returns the total number of navigable rows.
func (m Model) flagItems() int { return len(m.toolkits) + len(m.toggles) }

func (m Model) flagsKey(msg tea.KeyMsg) (tea.Model, tea.Cmd) {
	switch msg.String() {
	case "up", "k":
		if m.flagCursor > 0 {
			m.flagCursor--
		}
	case "down", "j":
		if m.flagCursor < m.flagItems()-1 {
			m.flagCursor++
		}
	case " ", "space", "x":
		m.toggleFlag()
	case "enter":
		return m.leaveFlags()
	}
	return m, nil
}

func (m *Model) toggleFlag() {
	if m.flagCursor < len(m.toolkits) {
		m.sel.Toolkit = m.toolkits[m.flagCursor].Key
		return
	}
	t := m.toggles[m.flagCursor-len(m.toolkits)]
	m.sel.Enabled[t.Key] = !m.sel.Enabled[t.Key]
}

func (m Model) leaveFlags() (tea.Model, tea.Cmd) {
	fam := m.info.Distro.Family
	m.pkgManager, _ = pkg.ForFamily(fam)
	res := emacs.Resolve(m.sel, m.env)
	m.configureArgs = res.Args
	m.logicalDeps = res.Deps
	m.resolution = pkg.Resolution{}
	m.resolving = m.pkgManager != nil
	m.step = stepDeps
	return m, resolveDeps(m.pkgManager, fam, res.Deps)
}

func (m Model) flagsView() string {
	var b strings.Builder
	b.WriteString(stHeading.Render("GUI toolkit") + "\n")
	for i, tk := range m.toolkits {
		focused := m.flagCursor == i
		on := m.sel.Toolkit == tk.Key
		label := tk.Title
		if focused {
			label = stSelected.Render(label)
		} else {
			label = stText.Render(label)
		}
		b.WriteString(cursor(focused) + radio(on, focused) + " " + label + "\n")
	}

	b.WriteString("\n" + stHeading.Render("Build features") + "\n")
	for i, t := range m.toggles {
		idx := len(m.toolkits) + i
		focused := m.flagCursor == idx
		on := m.sel.Enabled[t.Key]
		label := t.Title
		if focused {
			label = stSelected.Render(label)
		} else if on {
			label = stText.Render(label)
		} else {
			label = stMuted.Render(label)
		}
		b.WriteString(cursor(focused) + checkbox(on, focused) + " " + label + "\n")
	}

	// Contextual help for the focused row.
	help := m.focusedHelp()
	b.WriteString("\n" + stInfo.Render("› ") + stSubtle.Render(wrap(help, m.bodyWidth()-2)))

	footer := joinHints(keyHint("↑/↓", "move"), keyHint("space", "toggle"), keyHint("enter", "continue"), keyHint("esc", "back"))
	return m.frame(b.String(), footer)
}

func (m Model) focusedHelp() string {
	if m.flagCursor < len(m.toolkits) {
		return m.toolkits[m.flagCursor].Help
	}
	i := m.flagCursor - len(m.toolkits)
	if i >= 0 && i < len(m.toggles) {
		return m.toggles[i].Help
	}
	return ""
}

// --- dependencies -----------------------------------------------------------

func (m Model) depsKey(msg tea.KeyMsg) (tea.Model, tea.Cmd) {
	if m.resolving {
		return m, nil
	}
	switch msg.String() {
	case "enter", " ", "space":
		m.serviceCursor = 0
		m.step = stepService
	}
	return m, nil
}

func (m Model) depsView() string {
	var b strings.Builder
	b.WriteString(stHeading.Render("Dependencies") + "\n\n")

	if m.pkgManager == nil {
		b.WriteString(stWarn.Render("Your distribution wasn't recognised, so packages can't be installed automatically.") + "\n")
		b.WriteString(stSubtle.Render(wrap("You'll need the usual Emacs build dependencies (a C toolchain, GTK, GnuTLS, libgccjit, tree-sitter, image libraries…) installed by hand.", m.bodyWidth())))
		return m.frame(b.String(), joinHints(keyHint("enter", "continue anyway"), keyHint("esc", "back")))
	}

	if m.resolving {
		b.WriteString(m.spinner.View() + " " + stMuted.Render("searching the "+m.pkgManager.Name+" repositories for valid package names…"))
		return m.frame(b.String(), joinHints(keyHint("esc", "back")))
	}

	pkgs := m.resolution.Packages
	b.WriteString(stText.Render(fmt.Sprintf("%d packages will be installed with ", len(pkgs))) + stKey.Render(m.pkgManager.Name) + stText.Render(":") + "\n\n")
	b.WriteString(stSubtle.Render(wrap(strings.Join(pkgs, "  "), m.bodyWidth())) + "\n")

	if len(m.resolution.Unverified) > 0 {
		b.WriteString("\n" + stWarn.Render("⚠ Could not confirm these in the repos (will still be attempted):") + "\n")
		b.WriteString("  " + stSubtle.Render(strings.Join(m.resolution.Unverified, "  ")) + "\n")
	}

	esc := "run directly (already root)"
	if m.info.Escalation.Needed() {
		esc = "via " + m.info.Escalation.Tool
	}
	b.WriteString("\n" + stMuted.Render("Installation runs "+esc+"."))

	return m.frame(b.String(), joinHints(keyHint("enter", "continue"), keyHint("esc", "back")))
}

// --- service / integration --------------------------------------------------

func (m Model) serviceRows() []string {
	rows := []string{"daemon", "init", "symlink"}
	if m.info.OS == "linux" {
		rows = append(rows, "desktop")
	}
	return rows
}

func (m Model) serviceKey(msg tea.KeyMsg) (tea.Model, tea.Cmd) {
	rows := m.serviceRows()
	switch msg.String() {
	case "up", "k":
		if m.serviceCursor > 0 {
			m.serviceCursor--
		}
	case "down", "j":
		if m.serviceCursor < len(rows)-1 {
			m.serviceCursor++
		}
	case "left", "h":
		if rows[m.serviceCursor] == "init" && m.daemon && len(m.inits) > 0 {
			m.initCursor = (m.initCursor - 1 + len(m.inits)) % len(m.inits)
		}
	case "right", "l":
		if rows[m.serviceCursor] == "init" && m.daemon && len(m.inits) > 0 {
			m.initCursor = (m.initCursor + 1) % len(m.inits)
		}
	case " ", "space", "x":
		switch rows[m.serviceCursor] {
		case "daemon":
			m.daemon = !m.daemon
		case "symlink":
			m.symlink = !m.symlink
		case "desktop":
			m.desktop = !m.desktop
		}
	case "enter":
		m.step = stepReview
	}
	return m, nil
}

func (m Model) serviceView() string {
	var b strings.Builder
	b.WriteString(stHeading.Render("Daemon & desktop integration") + "\n\n")

	rows := m.serviceRows()
	for i, kind := range rows {
		focused := m.serviceCursor == i
		switch kind {
		case "daemon":
			b.WriteString(cursor(focused) + checkbox(m.daemon, focused) + " " + label(focused, m.daemon, "Enable the Emacs daemon at login (so emacsclient just works)") + "\n")
		case "init":
			cur := m.selectedInit()
			detected := ""
			if cur == m.info.Init {
				detected = stMuted.Render("  (detected)")
			}
			val := fmt.Sprintf("‹ %s ›", cur.Title())
			style := stMuted
			if m.daemon {
				style = stText
			}
			if focused {
				style = stSelected
			}
			b.WriteString(cursor(focused) + "    " + stSubtle.Render("service via ") + style.Render(val) + detected + "\n")
		case "symlink":
			b.WriteString(cursor(focused) + checkbox(m.symlink, focused) + " " + label(focused, m.symlink, "Symlink emacs & emacsclient into /usr/local/bin (adds them to PATH)") + "\n")
		case "desktop":
			b.WriteString(cursor(focused) + checkbox(m.desktop, focused) + " " + label(focused, m.desktop, "Create .desktop application launchers") + "\n")
		}
	}

	if m.daemon {
		b.WriteString("\n" + stInfo.Render("› ") + stSubtle.Render(wrap(service.Summary(m.serviceParams()), m.bodyWidth()-2)))
	}

	footer := joinHints(keyHint("↑/↓", "move"), keyHint("←/→", "init"), keyHint("space", "toggle"), keyHint("enter", "continue"), keyHint("esc", "back"))
	return m.frame(b.String(), footer)
}

func label(focused, on bool, text string) string {
	switch {
	case focused:
		return stSelected.Render(text)
	case on:
		return stText.Render(text)
	default:
		return stMuted.Render(text)
	}
}

// --- review -----------------------------------------------------------------

func (m Model) reviewKey(msg tea.KeyMsg) (tea.Model, tea.Cmd) {
	switch msg.String() {
	case "q":
		m.quitting = true
		return m, tea.Quit
	case "enter":
		initSys := sys.InitUnknown
		if m.daemon {
			initSys = m.selectedInit()
		}
		m.cfg = emacs.Config{
			Info:          m.info,
			Version:       m.selVersion(),
			Env:           m.env,
			Selection:     m.sel,
			Prefix:        m.prefix,
			Manager:       m.pkgManager,
			Packages:      m.resolution.Packages,
			ConfigureArgs: m.configureArgs,
			Symlink:       m.symlink,
			Desktop:       m.desktop,
			EnableDaemon:  m.daemon,
			Init:          initSys,
			DryRun:        m.dryRun,
		}
		m.phases = m.cfg.BuildPhases()
		m.logLines = nil
		m.step = stepInstall
		m.relayout()
		return m.startPhase(0)
	}
	return m, nil
}

func (m Model) reviewView() string {
	var b strings.Builder
	b.WriteString(stHeading.Render("Review") + "\n\n")

	v := m.selVersion()
	add := func(k, val string) {
		b.WriteString("  " + stSubtle.Render(fmt.Sprintf("%-12s", k)) + " " + stText.Render(val) + "\n")
	}
	add("Version", v.Title()+"  "+stMuted.Render("("+string(v.Channel)+")"))
	add("Toolkit", emacs.ToolkitByKey(m.env, m.sel.Toolkit).Title)
	add("Features", m.enabledFeatureList())
	add("Prefix", m.prefix)
	if m.pkgManager != nil {
		add("Packages", fmt.Sprintf("%d via %s", len(m.resolution.Packages), m.pkgManager.Name))
	} else {
		add("Packages", stWarn.Render("install manually (unknown distro)"))
	}
	if m.daemon {
		add("Daemon", "enabled — "+m.selectedInit().Title())
	} else {
		add("Daemon", "disabled")
	}
	add("Launchers", yesno(m.desktop && m.info.OS == "linux"))
	add("PATH link", yesno(m.symlink))

	b.WriteString("\n" + stSubtle.Render("configure ") + stMuted.Render(wrap(strings.Join(m.cfgArgsPreview(), " "), m.bodyWidth())) + "\n")

	if m.info.Escalation.Needed() {
		b.WriteString("\n" + stMuted.Render(fmt.Sprintf("Root steps run via %s; you may be prompted for a password.", m.info.Escalation.Tool)))
	} else if !m.info.Escalation.Available && !m.dryRun {
		b.WriteString("\n" + stErr.Render("⚠ No way to gain root — the install step will fail. Use --dry-run or run as root."))
	}
	if m.dryRun {
		b.WriteString("\n" + stInfo.Render("● Dry-run: the pipeline will print commands without changing anything."))
	}

	action := "install"
	if m.dryRun {
		action = "dry-run"
	}
	footer := joinHints(keyHint("enter", action), keyHint("esc", "back"), keyHint("q", "quit"))
	return m.frame(b.String(), footer)
}

func (m Model) cfgArgsPreview() []string {
	return append([]string{"--prefix=" + m.prefix}, m.configureArgs...)
}

func (m Model) enabledFeatureList() string {
	var on []string
	for _, t := range m.toggles {
		if m.sel.Enabled[t.Key] {
			on = append(on, strings.TrimSpace(strings.TrimPrefix(t.Title, "  ↳ ")))
		}
	}
	if len(on) == 0 {
		return "(minimal)"
	}
	return strings.Join(on, ", ")
}

// --- install (live) ---------------------------------------------------------

func (m Model) installView() string {
	var b strings.Builder

	title := "Working…"
	if m.phaseIdx < len(m.phases) {
		title = m.phases[m.phaseIdx].Title
	}
	pct := int(m.overallFrac()*100 + 0.5)
	b.WriteString(stHeading.Render("Installing Emacs") +
		stMuted.Render(fmt.Sprintf("   step %d of %d", min(m.phaseIdx+1, len(m.phases)), len(m.phases))) + "\n\n")
	b.WriteString(m.progress.ViewAs(m.overallFrac()) + stText.Render(fmt.Sprintf("  %3d%%", pct)) + "\n\n")
	b.WriteString(m.spinner.View() + " " + stSelected.Render(title) + "\n")

	logBox := lipgloss.NewStyle().
		Border(lipgloss.NormalBorder()).
		BorderForeground(cDim).
		Render(m.logView.View())
	b.WriteString("\n" + logBox)

	footer := joinHints(keyHint("↑/↓/wheel", "scroll log"), keyHint("ctrl+c", "abort"))
	return m.frame(b.String(), footer)
}

// --- done / error -----------------------------------------------------------

func (m Model) doneView() string {
	var b strings.Builder
	head := "Emacs installed"
	if m.dryRun {
		head = "Dry-run complete"
	}
	b.WriteString(stGood.Render("✓ "+head) + "\n\n")

	v := m.selVersion()
	b.WriteString(stText.Render(fmt.Sprintf("%s is installed in ", v.Title())) + stKey.Render(m.prefix) + ".\n\n")

	launch := m.prefix + "/bin/emacs"
	client := m.prefix + "/bin/emacsclient -c"
	if m.symlink {
		launch = "emacs"
		client = "emacsclient -c"
	}
	b.WriteString(stSubtle.Render("Launch the editor:      ") + stKey.Render(launch) + "\n")
	if m.daemon {
		b.WriteString(stSubtle.Render("Connect to the daemon:  ") + stKey.Render(client) + "\n")
		b.WriteString("\n" + stMuted.Render("The daemon is enabled via "+m.selectedInit().Title()+" and will start automatically."))
	} else {
		b.WriteString(stSubtle.Render("Client (start daemon):  ") + stKey.Render(client+` -a ""`) + "\n")
	}

	return m.frame(b.String(), joinHints(keyHint("enter/q", "quit")))
}

func (m Model) errorView() string {
	var b strings.Builder
	b.WriteString(stErr.Render("✗ Installation failed") + "\n\n")
	if m.installErr != nil {
		b.WriteString(stText.Render(wrap(m.installErr.Error(), m.bodyWidth())) + "\n")
	}
	b.WriteString("\n" + stMuted.Render("Recent output:") + "\n")
	tail := m.logLines
	if len(tail) > 8 {
		tail = tail[len(tail)-8:]
	}
	for _, l := range tail {
		b.WriteString(stSubtle.Render("  "+l) + "\n")
	}
	return m.frame(b.String(), joinHints(keyHint("enter/q", "quit")))
}

// --- small helpers ----------------------------------------------------------

func (m Model) serviceParams() service.Params {
	return service.Params{
		Init:     m.selectedInit(),
		User:     m.info.User,
		Home:     m.info.Home,
		EmacsBin: m.prefix + "/bin/emacs",
		Notify:   m.sel.Enabled["libsystemd"],
	}
}

func yesno(b bool) string {
	if b {
		return "yes"
	}
	return "no"
}
