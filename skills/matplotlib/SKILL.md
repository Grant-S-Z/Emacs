---
name: matplotlib
description: Create or edit Matplotlib figures using the project's or user's local matplotlibrc.
---

# Matplotlib

Before creating a figure, explicitly load the project's `.matplotlibrc` or `matplotlibrc` with `matplotlib.rc_file(path)`. If both exist in the same directory, prefer `.matplotlibrc`. If the project has neither, use Matplotlib's normal user configuration. For that fallback, `matplotlib.matplotlib_fname()` shows the default configuration path.

Let the configuration set fonts, colors, line widths, and DPI. Override those defaults only when the user requests it.
