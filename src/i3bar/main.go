package main

import (
	"os/user"
	"path/filepath"
	"time"

	"barista.run"
	"barista.run/bar"
	"barista.run/base/click"
	"barista.run/base/watchers/netlink"
	"barista.run/colors"
	"barista.run/format"
	"barista.run/modules/battery"
	"barista.run/modules/clock"
	"barista.run/modules/meminfo"
	"barista.run/modules/netspeed"
	"barista.run/modules/sysinfo"
	"barista.run/modules/wlan"
	"barista.run/outputs"
	"barista.run/pango"
	"barista.run/pango/icons/fontawesome"

	colorful "github.com/lucasb-eyer/go-colorful"
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

	colors.LoadBarConfig()
	bg := colors.Scheme("background")
	fg := colors.Scheme("statusline")
	if fg != nil && bg != nil {
		iconColor := fg.Colorful().BlendHcl(bg.Colorful(), 0.5).Clamped()
		colors.Set("dim-icon", iconColor)
		_, _, v := fg.Colorful().Hsv()
		if v < 0.3 {
			v = 0.3
		}
		colors.Set("bad", colorful.Hcl(40, 1.0, v).Clamped())
		colors.Set("degraded", colorful.Hcl(90, 1.0, v).Clamped())
		colors.Set("good", colorful.Hcl(120, 1.0, v).Clamped())
	}

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
		out := outputs.Pango(pango.Icon("fa-"+iconName), disp)
		switch {
		case percentage <= 5:
			out.Urgent(true)
		case percentage <= 15:
			out.Color(colors.Scheme("bad"))
		case percentage <= 25:
			out.Color(colors.Scheme("degraded"))
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
		// Load averages are unusually high for a few minutes after boot.
		if s.Uptime < 10*time.Minute {
			// so don't add colours until 10 minutes after system start.
			return out
		}
		switch {
		case s.Loads[0] > 128, s.Loads[2] > 64:
			out.Urgent(true)
		case s.Loads[0] > 64, s.Loads[2] > 32:
			out.Color(colors.Scheme("bad"))
		case s.Loads[0] > 32, s.Loads[2] > 16:
			out.Color(colors.Scheme("degraded"))
		}
		return out
	})

	freeMem := meminfo.New().Output(func(m meminfo.Info) bar.Output {
		out := outputs.Pango(pango.Icon("fa-memory"), spacer, format.IBytesize(m.Available()))
		freeGigs := m.Available().Gigabytes()
		switch {
		case freeGigs < 0.5:
			out.Urgent(true)
		case freeGigs < 1:
			out.Color(colors.Scheme("bad"))
		case freeGigs < 2:
			out.Color(colors.Scheme("degraded"))
		case freeGigs > 12:
			out.Color(colors.Scheme("good"))
		}
		return out
	})

	sub := netlink.Any()
	iface := sub.Get().Name
	sub.Unsubscribe()
	net := netspeed.New(iface).
		RefreshInterval(2 * time.Second).
		Output(func(s netspeed.Speeds) bar.Output {
			return outputs.Pango(
				pango.Icon("fa-upload"), spacer, pango.Textf("%7s", format.Byterate(s.Tx)),
				pango.Text(" ").Small(),
				pango.Icon("fa-download"), spacer, pango.Textf("%7s", format.Byterate(s.Rx)),
			)
		})

	wireless := wlan.Any().Output(
		func(i wlan.Info) bar.Output {
			if !i.Enabled() {
				return nil
			}

			out := outputs.Pango(
				pango.Icon("fa-wifi"),
				spacer,
				pango.Textf("%s (%0.2f GHz)", i.SSID, i.Frequency.Gigahertz()),
			)

			switch {
			case i.Connected():
				out.Color(colors.Scheme("good"))
			case i.Connecting():
				out.Color(colors.Scheme("degraded"))
			}

			return out
		})

	panic(barista.Run(
		net,
		wireless,
		freeMem,
		loadAvg,
		batt,
		localtime,
	))
}
