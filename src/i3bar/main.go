package main

import (
	"os/user"
	"path/filepath"
	"runtime"
	"time"

	"barista.run"
	"barista.run/bar"
	"barista.run/base/click"
	"barista.run/base/watchers/netlink"
	"barista.run/format"
	"barista.run/modules/battery"
	"barista.run/modules/clock"
	"barista.run/modules/meminfo"
	"barista.run/modules/sysinfo"
	"barista.run/modules/wlan"
	"barista.run/outputs"
	"barista.run/pango"
	"barista.run/pango/icons/fontawesome"
)

var spacer = pango.Text(" ").XXSmall()

func truncate(in string, l int) string {
	if len([]rune(in)) <= l {
		return in
	}
	return string([]rune(in)[:l-1]) + "⋯"
}

func hms(d time.Duration) (h int, m int, s int) {
	h = int(d.Hours())
	m = int(d.Minutes()) % 60
	s = int(d.Seconds()) % 60
	return
}

func home(path string) string {
	usr, err := user.Current()
	if err != nil {
		panic(err)
	}
	return filepath.Join(usr.HomeDir, path)
}

func main() {
	fontawesome.Load(home("src/Font-Awesome"))

	localtime := clock.Local().
		Output(time.Second, func(now time.Time) bar.Output {
			return outputs.Pango(
				now.Format("2006-01-02 15:04:05"),
			)
		})

	buildBattOutput := func(i battery.Info, disp *pango.Node) *bar.Segment {
		if i.Status == battery.Disconnected || i.Status == battery.Unknown {
			return nil
		}
		iconName := "battery"
		percentage := i.RemainingPct()
		switch {
		case percentage > 90:
			iconName += "-full"
		case percentage > 60:
			iconName += "-three-quarters"
		case percentage > 40:
			iconName += "-half"
		case percentage > 10:
			iconName += "-quarter"
		default:
			iconName += "-empty"
		}
		out := outputs.Pango(pango.Icon("fa-"+iconName), spacer, disp)
		if percentage <= 5 {
			out.Urgent(true)
		}
		return out
	}
	var showBattPct, showBattTime func(battery.Info) bar.Output

	batt := battery.All()
	showBattPct = func(i battery.Info) bar.Output {
		out := buildBattOutput(i, pango.Textf("%d%%", i.RemainingPct()))
		if out == nil {
			return nil
		}
		return out.OnClick(click.Left(func() {
			batt.Output(showBattTime)
		}))
	}
	showBattTime = func(i battery.Info) bar.Output {
		rem := i.RemainingTime()
		out := buildBattOutput(i, pango.Textf(
			"%d:%02d", int(rem.Hours()), int(rem.Minutes())%60))
		if out == nil {
			return nil
		}
		return out.OnClick(click.Left(func() {
			batt.Output(showBattPct)
		}))
	}
	batt.Output(showBattPct)

	loadAvg := sysinfo.New().Output(func(s sysinfo.Info) bar.Output {
		out := outputs.Pango(pango.Icon("fa-microchip"), spacer, pango.Textf("%0.2f %0.2f", s.Loads[0], s.Loads[2]))

		if s.Loads[0] > float64(runtime.NumCPU()) {
			out.Urgent(true)
		}

		return out
	})

	freeMem := meminfo.New().Output(func(m meminfo.Info) bar.Output {
		out := outputs.Pango(pango.Icon("fa-memory"), spacer, format.IBytesize(m.Available()))
		freeGigs := m.Available().Gigabytes()
		switch {
		case freeGigs < 0.5:
			out.Urgent(true)
		}
		return out
	})

	wireless := wlan.Any().Output(
		func(i wlan.Info) bar.Output {
			if !i.Enabled() {
				return nil
			}

			var out *bar.Segment

			if i.State == netlink.Up {
				out = outputs.Pango(
					pango.Icon("fa-wifi"),
					spacer,
					pango.Textf("%s (%0.2f GHz)", i.SSID, i.Frequency.Gigahertz()),
				)
			} else {
				out = outputs.Pango(
					pango.Icon("fa-wifi"),
					spacer,
					pango.Text("down"),
				)
				out.Urgent(true)
			}

			return out
		})

	panic(barista.Run(
		wireless,
		freeMem,
		loadAvg,
		batt,
		localtime,
	))
}
