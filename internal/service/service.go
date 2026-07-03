// Package service generates and enables an "emacs --daemon" service for the
// detected init system, so emacsclient works immediately and after every boot.
// It deliberately supports the whole spread of Linux init systems (systemd,
// OpenRC, runit, SysVinit, dinit) plus macOS launchd — not just systemd.
package service

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/Borderliner/emacs-installer/internal/action"
	"github.com/Borderliner/emacs-installer/internal/sys"
)

// Params describes the daemon service to generate.
type Params struct {
	Init     sys.InitSystem
	User     string // account the daemon runs as
	Home     string // that user's home (for user-scoped units)
	EmacsBin string // absolute path, e.g. /opt/emacs/bin/emacs
	Notify   bool   // build has libsystemd → systemd Type=notify
	RunDir   string // runit supervision dir (default /var/service)
}

func (p Params) clientBin() string {
	return filepath.Join(filepath.Dir(p.EmacsBin), "emacsclient")
}

// Generate returns the files to write and commands to run to install and enable
// the daemon. Whether items are System (root) or user-scoped depends on the
// init system: systemd and launchd get clean per-user units; the others need a
// system service that launches the daemon as the target user.
func Generate(p Params) ([]action.FileSpec, []action.Command, error) {
	switch p.Init {
	case sys.InitSystemd:
		return p.systemd()
	case sys.InitOpenRC:
		return p.openrc()
	case sys.InitRunit:
		return p.runit()
	case sys.InitSysV:
		return p.sysv()
	case sys.InitDinit:
		return p.dinit()
	case sys.InitLaunchd:
		return p.launchd()
	default:
		return nil, nil, fmt.Errorf("no service template for init system %q", p.Init)
	}
}

// Summary is a one-line description of what enabling the service will do, for
// the review screen.
func Summary(p Params) string {
	switch p.Init {
	case sys.InitSystemd:
		return "systemd user unit (~/.config/systemd/user/emacs.service), enabled with systemctl --user"
	case sys.InitOpenRC:
		return "OpenRC service /etc/init.d/emacs, added to the default runlevel"
	case sys.InitRunit:
		return fmt.Sprintf("runit service /etc/sv/emacs linked into %s", p.runDir())
	case sys.InitSysV:
		return "SysVinit script /etc/init.d/emacs, registered for the standard runlevels"
	case sys.InitDinit:
		return "dinit service /etc/dinit.d/emacs, enabled with dinitctl"
	case sys.InitLaunchd:
		return "launchd LaunchAgent (~/Library/LaunchAgents/gnu.emacs.daemon.plist)"
	default:
		return string(p.Init)
	}
}

func (p Params) runDir() string {
	if p.RunDir != "" {
		return p.RunDir
	}
	return "/var/service"
}

// Teardown returns everything needed to stop, disable and remove the daemon
// service: the file paths to delete (content is irrelevant here) and the
// stop/disable commands to run. Items are tagged System or user exactly as in
// Generate, so the uninstaller can route them correctly.
func Teardown(p Params) (remove []action.FileSpec, cmds []action.Command) {
	del := func(path string, system bool) action.FileSpec {
		return action.FileSpec{Path: path, System: system}
	}
	switch p.Init {
	case sys.InitSystemd:
		unit := filepath.Join(p.Home, ".config/systemd/user/emacs.service")
		return []action.FileSpec{del(unit, false)}, []action.Command{
			{Argv: []string{"systemctl", "--user", "disable", "--now", "emacs.service"}, Label: "stop & disable daemon"},
			{Argv: []string{"systemctl", "--user", "daemon-reload"}, Label: "reload user units"},
		}
	case sys.InitOpenRC:
		return []action.FileSpec{del("/etc/init.d/emacs", true)}, []action.Command{
			{Argv: []string{"rc-service", "emacs", "stop"}, System: true, Label: "stop daemon"},
			{Argv: []string{"rc-update", "del", "emacs", "default"}, System: true, Label: "disable at boot"},
		}
	case sys.InitRunit:
		return []action.FileSpec{
				del(filepath.Join(p.runDir(), "emacs"), true), // supervision symlink
				del("/etc/sv/emacs", true),                    // service directory
			}, []action.Command{
				{Argv: []string{"sv", "down", "emacs"}, System: true, Label: "stop daemon"},
			}
	case sys.InitSysV:
		disable := `if command -v update-rc.d >/dev/null 2>&1; then update-rc.d -f emacs remove; ` +
			`elif command -v chkconfig >/dev/null 2>&1; then chkconfig --del emacs; fi`
		return []action.FileSpec{del("/etc/init.d/emacs", true)}, []action.Command{
			{Argv: []string{"/etc/init.d/emacs", "stop"}, System: true, Label: "stop daemon"},
			{Argv: []string{"sh", "-c", disable}, System: true, Label: "deregister service"},
		}
	case sys.InitDinit:
		return []action.FileSpec{del("/etc/dinit.d/emacs", true)}, []action.Command{
			{Argv: []string{"dinitctl", "stop", "emacs"}, System: true, Label: "stop daemon"},
			{Argv: []string{"dinitctl", "disable", "emacs"}, System: true, Label: "disable service"},
		}
	case sys.InitLaunchd:
		plist := filepath.Join(p.Home, "Library/LaunchAgents/gnu.emacs.daemon.plist")
		return []action.FileSpec{del(plist, false)}, []action.Command{
			{Argv: []string{"launchctl", "unload", "-w", plist}, Label: "unload agent"},
		}
	default:
		return nil, nil
	}
}

// --- systemd (user scope) ---------------------------------------------------

func (p Params) systemd() ([]action.FileSpec, []action.Command, error) {
	stype := "simple"
	if p.Notify {
		stype = "notify"
	}
	unit := fmt.Sprintf(`[Unit]
Description=Emacs text editor daemon
Documentation=info:emacs man:emacs(1) https://gnu.org/software/emacs/
After=default.target

[Service]
Type=%s
ExecStart=%s --fg-daemon
ExecStop=%s --no-wait --eval "(kill-emacs)"
Restart=on-failure

[Install]
WantedBy=default.target
`, stype, p.EmacsBin, p.clientBin())

	path := filepath.Join(p.Home, ".config/systemd/user/emacs.service")
	specs := []action.FileSpec{{
		Path: path, Content: unit, Mode: 0o644, System: false,
		Label: "systemd user unit",
	}}
	cmds := []action.Command{
		{Argv: []string{"systemctl", "--user", "daemon-reload"}, System: false, Label: "reload user units"},
		{Argv: []string{"systemctl", "--user", "enable", "--now", "emacs.service"}, System: false, Label: "enable emacs daemon"},
	}
	return specs, cmds, nil
}

// --- OpenRC (system scope) --------------------------------------------------

func (p Params) openrc() ([]action.FileSpec, []action.Command, error) {
	script := fmt.Sprintf(`#!/sbin/openrc-run
name="emacs"
description="Emacs editor daemon"
command="%s"
command_args="--fg-daemon"
command_user="%s"
command_background=true
pidfile="/run/emacs.pid"
output_log="/var/log/emacs.log"
error_log="/var/log/emacs.log"

depend() {
	need localmount
	after bootmisc
}
`, p.EmacsBin, p.User)

	specs := []action.FileSpec{{
		Path: "/etc/init.d/emacs", Content: script, Mode: 0o755, System: true,
		Label: "OpenRC service",
	}}
	cmds := []action.Command{
		{Argv: []string{"rc-update", "add", "emacs", "default"}, System: true, Label: "enable at boot"},
		{Argv: []string{"rc-service", "emacs", "start"}, System: true, Label: "start now"},
	}
	return specs, cmds, nil
}

// --- runit (system scope) ---------------------------------------------------

func (p Params) runit() ([]action.FileSpec, []action.Command, error) {
	run := fmt.Sprintf(`#!/bin/sh
exec 2>&1
exec chpst -u %s %s --fg-daemon
`, p.User, p.EmacsBin)

	specs := []action.FileSpec{{
		Path: "/etc/sv/emacs/run", Content: run, Mode: 0o755, System: true,
		Label: "runit run script",
	}}
	link := filepath.Join(p.runDir(), "emacs")
	cmds := []action.Command{
		{Argv: []string{"ln", "-sfn", "/etc/sv/emacs", link}, System: true, Label: "enable service"},
	}
	return specs, cmds, nil
}

// --- SysVinit (system scope) ------------------------------------------------

func (p Params) sysv() ([]action.FileSpec, []action.Command, error) {
	script := fmt.Sprintf(`#!/bin/sh
### BEGIN INIT INFO
# Provides:          emacs
# Required-Start:    $local_fs $remote_fs $network
# Required-Stop:     $local_fs $remote_fs
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# Short-Description: Emacs editor daemon
### END INIT INFO
DAEMON=%s
CLIENT=%s
USER=%s
case "$1" in
	start)   su - "$USER" -c "$DAEMON --daemon" ;;
	stop)    su - "$USER" -c "$CLIENT --no-wait --eval '(kill-emacs)'" ;;
	restart) "$0" stop; "$0" start ;;
	*) echo "Usage: $0 {start|stop|restart}"; exit 1 ;;
esac
`, p.EmacsBin, p.clientBin(), p.User)

	specs := []action.FileSpec{{
		Path: "/etc/init.d/emacs", Content: script, Mode: 0o755, System: true,
		Label: "SysVinit script",
	}}
	// update-rc.d (Debian) or chkconfig (RHEL), whichever exists.
	enable := `if command -v update-rc.d >/dev/null 2>&1; then update-rc.d emacs defaults; ` +
		`elif command -v chkconfig >/dev/null 2>&1; then chkconfig --add emacs; fi`
	cmds := []action.Command{
		{Argv: []string{"sh", "-c", enable}, System: true, Label: "register service"},
		{Argv: []string{"/etc/init.d/emacs", "start"}, System: true, Label: "start now"},
	}
	return specs, cmds, nil
}

// --- dinit (system scope) ---------------------------------------------------

func (p Params) dinit() ([]action.FileSpec, []action.Command, error) {
	svc := fmt.Sprintf(`type = process
command = %s --fg-daemon
run-as = %s
restart = true
smooth-recovery = true
logfile = /var/log/emacs.log
`, p.EmacsBin, p.User)

	specs := []action.FileSpec{{
		Path: "/etc/dinit.d/emacs", Content: svc, Mode: 0o644, System: true,
		Label: "dinit service",
	}}
	cmds := []action.Command{
		{Argv: []string{"dinitctl", "enable", "emacs"}, System: true, Label: "enable service"},
	}
	return specs, cmds, nil
}

// --- launchd (macOS, user scope) --------------------------------------------

func (p Params) launchd() ([]action.FileSpec, []action.Command, error) {
	home := p.Home
	if home == "" {
		home = os.Getenv("HOME")
	}
	plist := fmt.Sprintf(`<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
	<key>Label</key><string>gnu.emacs.daemon</string>
	<key>ProgramArguments</key>
	<array>
		<string>%s</string>
		<string>--fg-daemon</string>
	</array>
	<key>RunAtLoad</key><true/>
	<key>KeepAlive</key><true/>
	<key>StandardErrorPath</key><string>/tmp/emacs-daemon.log</string>
	<key>StandardOutPath</key><string>/tmp/emacs-daemon.log</string>
</dict>
</plist>
`, p.EmacsBin)

	path := filepath.Join(home, "Library/LaunchAgents/gnu.emacs.daemon.plist")
	specs := []action.FileSpec{{
		Path: path, Content: plist, Mode: 0o644, System: false,
		Label: "launchd agent",
	}}
	cmds := []action.Command{
		{Argv: []string{"launchctl", "load", "-w", path}, System: false, Label: "load agent"},
	}
	return specs, cmds, nil
}
