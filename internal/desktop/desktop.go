// Package desktop generates freedesktop.org .desktop launchers for a
// prefix-installed Emacs. Because Emacs lives under /opt/emacs (off the default
// XDG search path), the entries use absolute Exec and Icon paths so menus and
// icons resolve correctly.
package desktop

import (
	"fmt"
	"path/filepath"

	"github.com/Borderliner/emacs-installer/internal/action"
)

// Generate returns the .desktop files for a standalone Emacs and an Emacs
// client. When system is true they are installed into /usr/share/applications
// (root); otherwise into the user's ~/.local/share/applications.
func Generate(prefix string, system bool, home string) []action.FileSpec {
	bin := filepath.Join(prefix, "bin")
	emacs := filepath.Join(bin, "emacs")
	client := filepath.Join(bin, "emacsclient")
	icon := filepath.Join(prefix, "share/icons/hicolor/scalable/apps/emacs.svg")

	dir := filepath.Join(home, ".local/share/applications")
	if system {
		dir = "/usr/share/applications"
	}

	emacsEntry := fmt.Sprintf(`[Desktop Entry]
Name=Emacs
GenericName=Text Editor
Comment=Edit text with GNU Emacs
Exec=%s %%F
Icon=%s
Type=Application
Terminal=false
Categories=Development;TextEditor;Utility;
MimeType=text/english;text/plain;text/x-makefile;text/x-c;text/x-c++;text/x-java;text/x-python;text/x-lisp;text/x-markdown;
StartupWMClass=Emacs
Keywords=Text;Editor;Lisp;
`, emacs, icon)

	clientEntry := fmt.Sprintf(`[Desktop Entry]
Name=Emacs (Client)
GenericName=Text Editor
Comment=Edit text in the running Emacs daemon
Exec=%s -c --alternate-editor=%s %%F
Icon=%s
Type=Application
Terminal=false
Categories=Development;TextEditor;Utility;
MimeType=text/english;text/plain;
StartupWMClass=Emacs
Keywords=Text;Editor;Lisp;Client;
`, client, emacs, icon)

	return []action.FileSpec{
		{Path: filepath.Join(dir, "emacs.desktop"), Content: emacsEntry, Mode: 0o644, System: system, Label: "Emacs launcher"},
		{Path: filepath.Join(dir, "emacsclient.desktop"), Content: clientEntry, Mode: 0o644, System: system, Label: "Emacs client launcher"},
	}
}
