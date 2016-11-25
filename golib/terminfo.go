package main

import (
	"fmt"
	"os/exec"
)

var terminals = map[string]string{
	"xterm":         "xterm",
	"rxvt-256color": "rxvt_256color",
	"rxvt-unicode":  "rxvt_unicode",
	"linux":         "linux",
	"Eterm":         "eterm",
	"screen":        "screen",
}

var keys = map[string]string{
	"F1":        "kf1",
	"F2":        "kf2",
	"F3":        "kf3",
	"F4":        "kf4",
	"F5":        "kf5",
	"F6":        "kf6",
	"F7":        "kf7",
	"F8":        "kf8",
	"F9":        "kf9",
	"F10":       "kf10",
	"F11":       "kf11",
	"F12":       "kf12",
	"INSERT":    "kich1",
	"DELETE":    "kdch1",
	"HOME":      "khome",
	"END":       "kend",
	"PGUP":      "kpp",
	"PGDN":      "knp",
	"KEY_UP":    "kcuu1",
	"KEY_DOWN":  "kcud1",
	"KEY_LEFT":  "kcub1",
	"KEY_RIGHT": "kcuf1",
}

var funcs = map[string]string{
	"T_ENTER_CA":     "smcup",
	"T_EXIT_CA":      "rmcup",
	"T_SHOW_CURSOR":  "cnorm",
	"T_HIDE_CURSOR":  "civis",
	"T_CLEAR_SCREEN": "clear",
	"T_SGR0":         "sgr0",
	"T_UNDERLINE":    "smul",
	"T_BOLD":         "bold",
	"T_BLINK":        "blink",
	"T_REVERSE":      "rev",
	"T_ENTER_KEYPAD": "smkx",
	"T_EXIT_KEYPAD":  "rmkx",
}

func tput(term, name string) {
	cmd := []string{"tput", "-T", term, name}
	out, err := exec.Command("tput", "-T", term, name).Output()
	if err != nil {
		fmt.Println(cmd, out, string(out))
	} else {
		fmt.Println("ERR: ", cmd, out, err)
	}
}

func Keys(term string) {
	for _, v := range keys {
		tput(term, v)
	}
}

func Funcs(term string) {
	for _, v := range funcs {
		tput(term, v)
	}
}

func main() {
	for _, v := range terminals {
		Keys(v)
	}
	for _, v := range terminals {
		Funcs(v)
	}
	// tput("xterm", "reset")
}
