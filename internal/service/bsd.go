package service

import (
	"fmt"

	"github.com/Borderliner/emacs-installer/internal/action"
)

// --- FreeBSD rc.d -----------------------------------------------------------

func (p Params) freebsd() ([]action.FileSpec, []action.Command, error) {
	script := fmt.Sprintf(`#!/bin/sh
# PROVIDE: emacs
# REQUIRE: LOGIN
# KEYWORD: shutdown
. /etc/rc.subr
name="emacs"
rcvar="emacs_enable"
command="%s"
command_args="--daemon"
command_user="%s"
: ${emacs_enable:="NO"}
load_rc_config $name
run_rc_command "$1"
`, p.EmacsBin, p.User)

	specs := []action.FileSpec{{
		Path: "/usr/local/etc/rc.d/emacs", Content: script, Mode: 0o755, System: true,
		Label: "FreeBSD rc.d service",
	}}
	cmds := []action.Command{
		{Argv: []string{"sysrc", "emacs_enable=YES"}, System: true, Label: "enable at boot"},
		{Argv: []string{"service", "emacs", "start"}, System: true, Label: "start now"},
	}
	return specs, cmds, nil
}

// --- OpenBSD rc.d -----------------------------------------------------------

func (p Params) openbsd() ([]action.FileSpec, []action.Command, error) {
	script := fmt.Sprintf(`#!/bin/ksh
daemon="%s"
daemon_flags="--daemon"
daemon_user="%s"

. /etc/rc.d/rc.subr

rc_bg=YES
rc_cmd $1
`, p.EmacsBin, p.User)

	specs := []action.FileSpec{{
		Path: "/etc/rc.d/emacs", Content: script, Mode: 0o755, System: true,
		Label: "OpenBSD rc.d service",
	}}
	cmds := []action.Command{
		{Argv: []string{"rcctl", "enable", "emacs"}, System: true, Label: "enable at boot"},
		{Argv: []string{"rcctl", "start", "emacs"}, System: true, Label: "start now"},
	}
	return specs, cmds, nil
}

// --- NetBSD rc.d ------------------------------------------------------------

func (p Params) netbsd() ([]action.FileSpec, []action.Command, error) {
	script := fmt.Sprintf(`#!/bin/sh
# PROVIDE: emacs
# REQUIRE: LOGIN
. /etc/rc.subr
name="emacs"
rcvar=$name
command="%s"
command_args="--daemon"
command_user="%s"
load_rc_config $name
run_rc_command "$1"
`, p.EmacsBin, p.User)

	enable := `grep -q '^emacs=' /etc/rc.conf 2>/dev/null || echo 'emacs=YES' >> /etc/rc.conf`
	specs := []action.FileSpec{{
		Path: "/etc/rc.d/emacs", Content: script, Mode: 0o755, System: true,
		Label: "NetBSD rc.d service",
	}}
	cmds := []action.Command{
		{Argv: []string{"sh", "-c", enable}, System: true, Label: "enable in rc.conf"},
		{Argv: []string{"/etc/rc.d/emacs", "start"}, System: true, Label: "start now"},
	}
	return specs, cmds, nil
}

// --- Slackware (BSD-style rc, no service manager) ---------------------------

func (p Params) slackware() ([]action.FileSpec, []action.Command, error) {
	script := fmt.Sprintf(`#!/bin/sh
# Emacs daemon control (Slackware).
CLIENT=%s
case "$1" in
	start)   su - %s -c "%s --daemon" ;;
	stop)    su - %s -c "$CLIENT --no-wait --eval '(kill-emacs)'" ;;
	restart) "$0" stop; "$0" start ;;
	*) echo "usage: $0 {start|stop|restart}" ;;
esac
`, p.clientBin(), p.User, p.EmacsBin, p.User)

	// Slackware runs /etc/rc.d/rc.local at boot if it is executable; hook our
	// service in there (idempotently).
	enable := `touch /etc/rc.d/rc.local && chmod +x /etc/rc.d/rc.local; ` +
		`grep -q rc.emacs /etc/rc.d/rc.local || printf '\n[ -x /etc/rc.d/rc.emacs ] && /etc/rc.d/rc.emacs start\n' >> /etc/rc.d/rc.local`

	specs := []action.FileSpec{{
		Path: "/etc/rc.d/rc.emacs", Content: script, Mode: 0o755, System: true,
		Label: "Slackware rc script",
	}}
	cmds := []action.Command{
		{Argv: []string{"sh", "-c", enable}, System: true, Label: "hook into rc.local"},
		{Argv: []string{"/etc/rc.d/rc.emacs", "start"}, System: true, Label: "start now"},
	}
	return specs, cmds, nil
}
