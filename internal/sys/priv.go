package sys

// Escalation describes how we gain root for the privileged steps (installing
// packages, `make install` into /opt, writing system service files). We support
// both sudo and doas; when already root, no wrapping is needed.
type Escalation struct {
	Tool      string // "sudo", "doas", or "" when already root
	Available bool   // false only when unprivileged and neither tool exists
}

// DetectEscalation picks a privilege-escalation strategy. prefer optionally
// forces "sudo" or "doas"; otherwise sudo wins when both are present because it
// is the more widely configured of the two.
func DetectEscalation(prefer string) Escalation {
	if IsRoot() {
		return Escalation{Tool: "", Available: true}
	}
	if prefer != "" && have(prefer) {
		return Escalation{Tool: prefer, Available: true}
	}
	if have("sudo") {
		return Escalation{Tool: "sudo", Available: true}
	}
	if have("doas") {
		return Escalation{Tool: "doas", Available: true}
	}
	return Escalation{Tool: "", Available: false}
}

// Needed reports whether commands must be wrapped to gain privilege.
func (e Escalation) Needed() bool { return e.Tool != "" }

// Label is a short human description for the review screen.
func (e Escalation) Label() string {
	switch {
	case !e.Available:
		return "none available"
	case e.Tool == "":
		return "already root"
	default:
		return e.Tool
	}
}

// Wrap prefixes an argv with the escalation tool when required.
func (e Escalation) Wrap(argv []string) []string {
	if e.Tool == "" {
		return argv
	}
	return append([]string{e.Tool}, argv...)
}

// WrapShell builds an argv that runs a shell script with privilege. Batching
// every privileged action into a single invocation means at most one password
// prompt per phase, which is what keeps doas (no credential cache by default)
// usable from inside the TUI.
func (e Escalation) WrapShell(script string) []string {
	switch e.Tool {
	case "":
		return []string{"sh", "-c", script}
	default:
		return []string{e.Tool, "sh", "-c", script}
	}
}
