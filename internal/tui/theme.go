package tui

import (
	"strings"

	"github.com/charmbracelet/lipgloss"
)

// A Catppuccin-Mocha-ish palette: soft, high-contrast on dark terminals, and
// pleasant in truecolor.
var (
	cAccent  = lipgloss.Color("#CBA6F7") // mauve — primary accent
	cAccent2 = lipgloss.Color("#F5C2E7") // pink — secondary accent
	cText    = lipgloss.Color("#CDD6F4")
	cSubtle  = lipgloss.Color("#A6ADC8")
	cMuted   = lipgloss.Color("#6C7086")
	cDim     = lipgloss.Color("#45475A")
	cGood    = lipgloss.Color("#A6E3A1")
	cWarn    = lipgloss.Color("#F9E2AF")
	cErr     = lipgloss.Color("#F38BA8")
	cInfo    = lipgloss.Color("#89DCEB")
)

var (
	stTitle    = lipgloss.NewStyle().Foreground(cAccent).Bold(true)
	stSubtitle = lipgloss.NewStyle().Foreground(cMuted)
	stText     = lipgloss.NewStyle().Foreground(cText)
	stSubtle   = lipgloss.NewStyle().Foreground(cSubtle)
	stMuted    = lipgloss.NewStyle().Foreground(cMuted)
	stHeading  = lipgloss.NewStyle().Foreground(cAccent2).Bold(true)
	stSelected = lipgloss.NewStyle().Foreground(cAccent).Bold(true)
	stGood     = lipgloss.NewStyle().Foreground(cGood)
	stWarn     = lipgloss.NewStyle().Foreground(cWarn)
	stErr      = lipgloss.NewStyle().Foreground(cErr)
	stInfo     = lipgloss.NewStyle().Foreground(cInfo)
	stKey      = lipgloss.NewStyle().Foreground(cAccent2).Bold(true)

	stCard = lipgloss.NewStyle().
		Border(lipgloss.RoundedBorder()).
		BorderForeground(cAccent).
		Padding(1, 3)

	stBadge = lipgloss.NewStyle().
		Foreground(lipgloss.Color("#11111B")).
		Background(cAccent).
		Bold(true).
		Padding(0, 1)
)

// checkbox renders a [✓]/[ ] box, coloured by state and focus.
func checkbox(on, focused bool) string {
	mark := " "
	style := stMuted
	if on {
		mark = "✓"
		style = stGood
	}
	if focused {
		style = stSelected
	}
	return style.Render("[" + mark + "]")
}

// radio renders a (●)/( ) selector.
func radio(on, focused bool) string {
	mark := " "
	style := stMuted
	if on {
		mark = "●"
		style = stAccentStyle()
	}
	if focused {
		style = stSelected
	}
	return style.Render("(" + mark + ")")
}

func stAccentStyle() lipgloss.Style { return lipgloss.NewStyle().Foreground(cAccent) }

// cursor returns the left-margin pointer for a row.
func cursor(focused bool) string {
	if focused {
		return stSelected.Render("▸ ")
	}
	return "  "
}

// keyHint formats a "key action" pair for the footer.
func keyHint(key, action string) string {
	return stKey.Render(key) + " " + stMuted.Render(action)
}

// joinHints joins footer hints with a dim separator.
func joinHints(hints ...string) string {
	return strings.Join(hints, stDim(" · "))
}

func stDim(s string) string { return lipgloss.NewStyle().Foreground(cDim).Render(s) }

// breadcrumb renders the step indicator (● current, ✓ done, ○ upcoming).
func breadcrumb(steps []string, current int) string {
	var parts []string
	for i, s := range steps {
		switch {
		case i < current:
			parts = append(parts, stGood.Render("✓ "+s))
		case i == current:
			parts = append(parts, stSelected.Render("● "+s))
		default:
			parts = append(parts, stMuted.Render("○ "+s))
		}
	}
	return strings.Join(parts, stDim("  ›  "))
}
