// Package action is a tiny, dependency-free vocabulary shared by the modules
// that need to create files and run commands during installation (services,
// desktop entries, symlinks). Each item is tagged System (needs root) or not,
// so the executor can batch all root work into a single privileged shell script
// while applying user-level work directly.
package action

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

// FileSpec is a file to create with fixed content and permissions.
type FileSpec struct {
	Path    string
	Content string
	Mode    os.FileMode
	System  bool   // true → written by the privileged script
	Label   string // short human description for logs
}

// Command is a command to run as part of integration (e.g. enabling a service).
type Command struct {
	Argv   []string
	System bool // true → run inside the privileged script
	Label  string
}

// ShellScript renders every System file and System command into one root shell
// program, suitable for a single sudo/doas invocation.
func ShellScript(specs []FileSpec, cmds []Command) string {
	var b strings.Builder
	for _, s := range specs {
		if s.System {
			writeHeredoc(&b, s)
		}
	}
	for _, c := range cmds {
		if c.System {
			b.WriteString(ShellJoin(c.Argv))
			b.WriteByte('\n')
		}
	}
	return b.String()
}

// HasSystem reports whether any item needs root.
func HasSystem(specs []FileSpec, cmds []Command) bool {
	for _, s := range specs {
		if s.System {
			return true
		}
	}
	for _, c := range cmds {
		if c.System {
			return true
		}
	}
	return false
}

// HasUser reports whether any item is user-scoped (applied without root).
func HasUser(specs []FileSpec, cmds []Command) bool {
	for _, s := range specs {
		if !s.System {
			return true
		}
	}
	for _, c := range cmds {
		if !c.System {
			return true
		}
	}
	return false
}

// ApplyUser writes the non-System files and runs the non-System commands in the
// current (unprivileged) process, reporting each step through emit.
func ApplyUser(specs []FileSpec, cmds []Command, emit func(string)) error {
	for _, s := range specs {
		if s.System {
			continue
		}
		if err := os.MkdirAll(filepath.Dir(s.Path), 0o755); err != nil {
			return err
		}
		if err := os.WriteFile(s.Path, []byte(s.Content), s.Mode.Perm()); err != nil {
			return err
		}
		emit("wrote " + s.Path)
	}
	for _, c := range cmds {
		if c.System || len(c.Argv) == 0 {
			continue
		}
		emit("$ " + strings.Join(c.Argv, " "))
		out, err := exec.Command(c.Argv[0], c.Argv[1:]...).CombinedOutput()
		for _, ln := range strings.Split(strings.TrimRight(string(out), "\n"), "\n") {
			if ln != "" {
				emit(ln)
			}
		}
		if err != nil {
			return fmt.Errorf("%s: %w", c.Argv[0], err)
		}
	}
	return nil
}

func writeHeredoc(b *strings.Builder, s FileSpec) {
	fmt.Fprintf(b, "mkdir -p %s\n", ShellQuote(filepath.Dir(s.Path)))
	fmt.Fprintf(b, "cat > %s <<'EI_EOF'\n", ShellQuote(s.Path))
	b.WriteString(s.Content)
	if !strings.HasSuffix(s.Content, "\n") {
		b.WriteByte('\n')
	}
	b.WriteString("EI_EOF\n")
	fmt.Fprintf(b, "chmod %o %s\n", s.Mode.Perm(), ShellQuote(s.Path))
}

// ShellQuote single-quotes a string for safe embedding in an sh program.
func ShellQuote(s string) string {
	return "'" + strings.ReplaceAll(s, "'", `'\''`) + "'"
}

// ShellJoin quotes and joins an argv into a shell command line.
func ShellJoin(argv []string) string {
	parts := make([]string, len(argv))
	for i, a := range argv {
		parts[i] = ShellQuote(a)
	}
	return strings.Join(parts, " ")
}
