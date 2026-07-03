package sys

import (
	"os"
	"runtime"
	"strings"
)

// InitSystem identifies the service manager we should generate a daemon unit
// for. We deliberately support the non-systemd world too.
type InitSystem string

const (
	InitSystemd InitSystem = "systemd"
	InitOpenRC  InitSystem = "openrc"
	InitRunit   InitSystem = "runit"
	InitSysV    InitSystem = "sysvinit"
	InitDinit   InitSystem = "dinit"
	InitLaunchd InitSystem = "launchd" // macOS
	InitUnknown InitSystem = "unknown"
)

// Title is a human-facing label for the init system.
func (i InitSystem) Title() string {
	switch i {
	case InitSystemd:
		return "systemd"
	case InitOpenRC:
		return "OpenRC"
	case InitRunit:
		return "runit"
	case InitSysV:
		return "SysVinit"
	case InitDinit:
		return "dinit"
	case InitLaunchd:
		return "launchd"
	default:
		return "unknown"
	}
}

// DetectInit determines the running service manager. It trusts PID 1's name
// first (the most reliable signal), then falls back to well-known filesystem
// and binary markers for setups where PID 1 is a generic /sbin/init.
func DetectInit() InitSystem {
	if runtime.GOOS == "darwin" {
		return InitLaunchd
	}

	switch pid1Comm() {
	case "systemd":
		return InitSystemd
	case "runit", "runsvdir":
		return InitRunit
	case "dinit":
		return InitDinit
	case "openrc-init":
		return InitOpenRC
	case "s6-svscan":
		// s6 is not a generation target; treat as unknown so the user picks.
		return InitUnknown
	}

	switch {
	case exists("/run/systemd/system"):
		return InitSystemd
	case exists("/run/openrc") || (have("openrc") && have("rc-update")):
		return InitOpenRC
	case have("sv") && (exists("/etc/runit") || exists("/run/runit")):
		return InitRunit
	case have("dinitctl") || exists("/etc/dinit.d"):
		return InitDinit
	case exists("/etc/init.d"):
		return InitSysV
	default:
		return InitUnknown
	}
}

// AvailableInits lists every init system we can generate a unit for on this
// host, so the UI can offer a manual override when detection is ambiguous
// (Artix, for example, ships several).
func AvailableInits() []InitSystem {
	if runtime.GOOS == "darwin" {
		return []InitSystem{InitLaunchd}
	}
	var out []InitSystem
	add := func(i InitSystem, ok bool) {
		if ok {
			out = append(out, i)
		}
	}
	add(InitSystemd, have("systemctl") || exists("/run/systemd/system"))
	add(InitOpenRC, have("rc-update") || have("openrc"))
	add(InitRunit, have("sv"))
	add(InitDinit, have("dinitctl"))
	add(InitSysV, exists("/etc/init.d") && (have("update-rc.d") || have("chkconfig") || have("service")))
	return out
}

// pid1Comm returns the executable name of PID 1 (e.g. "systemd", "runit").
func pid1Comm() string {
	b, err := os.ReadFile("/proc/1/comm")
	if err != nil {
		return ""
	}
	return strings.TrimSpace(string(b))
}
