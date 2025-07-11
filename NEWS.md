# praatpicture 1.5.0
* Added arguments `pitch_plotOnWave` and `intensity_plotOnWave` for overlaying pitch and intensity on waveforms, analogous to the `pitch_plotOnSpec` and `intensity_plotOnSpec` arguments. Since the code required for this is very similar for spectrograms and waveforms, I did some refactoring to avoid repeating code. (Thanks to Janne Lorenzen for the suggestion!)
* Added Shiny support for waveform overlays and for controlling the y-axis digits in waveforms.
* Long overdue updates to some of the documentation.

# praatpicture 1.4.4
* Added argument `wave_axisDigits` to determine how many digits should be used
for the waveform y-axis when `min_max_only = TRUE`. Passing `0` will suppress
the y-axis. (Thanks to Josie Riverin-Coutlée for the suggestion!)

# praatpicture 1.4.3
* Fixed bug that broke TextGrid-delimited highlighting

# praatpicture 1.4.2
* Added Shiny support for background highlighting.

# praatpicture 1.4.1
* Added support for a `background` argument in the `*_highlight` arguments.
* Added `tg_edgeLabels` argument for controlling the behavior of TextGrid 
intervals what fall partially outside of the plotting area.
* Fixed a bug with the `*_highlight` arguments leading to crashes when there
were TextGrid matches outside the plotting area.

# praatpicture 1.4.0
* Added a range of arguments to `praatpicture()` and sister functions for 
differential highlighting of signals with colors, line width, point size to 
stand out from the rest of the plot. These are `highlight` (for all signals),
`wave_highlight`, `spec_highlight`, `tg_highlight`, `pitch_highlight`, 
`formant_highlight`, and `intensity_highlight`. These should be named `list()`s
with information about which part of the signal to highlight, using either 
`start` or `end` arguments, or using `tier` and `label` arguments if 
highlighting is based on information in a TextGrid. Other possible arguments
are `color`, `drawSize`, and `speckleSize`, which work in the usual way.
Shiny support has been added, but is somewhat simplified.

# praatpicture 1.3.1
* Fix to `emupicture()`. In previous versions, all annotation levels were
converted to a TextGrid, which could have unfortunate side effects if some 
levels have no time information. Changed so that when the `tg_tiers` argument is 
set, only those annotation levels are exported.

# praatpicture 1.3.0
* Added the `drawSize` and `speckleSize` arguments, which respectively control
the line widths of plot components where the `_plotType` is `'draw'` and the
point sizes of plot components where the `_plotType` is `'speckle'`.
* Added the `wave_lineWidth` argument which controls the line width of the
waveform.
* Added the `tUnit` argument for controlling the time unit printed along the
x-axis. The default is still time in seconds, but when `tUnit = 'ms'`, the
time unit is milliseconds. The `time_axisLabel` argument will by default reflect
the unit specified in `tUnit`, but the axis label can still be controlled by 
the user.
* Added the `formant_number` argument which determines the number of formants
to include in a plot (independently of `formant_maxN`, which determines the
number of formants to track if no `.Formant` file is provided). By default,
all available formants are plotted. 
* Added the `mainTitleAlignment` argument which controls the vertical 
alignment of the plot title.
* Added support for all the above features in `shiny_praatpicture()`
* Fixed a bug that made it impossible to plot 24-bit WAV files.

# praatpicture 1.2.4
* Added the option to plot read and plot `Pitch` files with the `.Pitch` 
extension generated in R in addition to `PitchTier` files with the `.PitchTier`
extension. If a `.Pitch` file is available with the same base name as the
plotted sound file, this is now by default used to when plotting pitch unless 
a `.PitchTier` file is also available. Plotting a `.Pitch` file is likely 
somewhat slower than plotting a `.PitchTier` file, but they are simpler to 
generate and this fixes the bug where plotting `.PitchTier` files with 
`pitch_plotType = 'draw'` would linearly extrapolate pitch in unvoiced regions.


# praatpicture 1.2.3
* Added `Blackman` as a possible option to `spec_windowShape`. Blackman windows
were already available in `phonTools::windowfunc()` which is used to generate
windows for spectrogram estimation. These weren't originally used by 
`praatpicture()` as they're not implemented in Praat, but I decided to add 
them because the Gaussian windows used by `phonTools::windowfunc()` don't
behave the same as Praat. If you the spectrogram of a simple sine wave using
the Gaussian windows, sidelobes (horizontal lines) are clearly visible in the
spectrogram; these are artefacts of the windowing and should theoretically
not be there when using Gaussian windows (at least according to the Praat 
documentation), but for some reason they are. 
* Increased the upload limit for the Shiny server to approx 50 MB.

# praatpicture 1.2.2
* Added `talking` argument to `emupicture()`, allowing users to make
`talking_praatpicture()` plots with embedded audio directly from an EMU 
database.

# praatpicture 1.2.1
* Fixed color issue when plotting point tiers in `tgplot()`.

# praatpicture 1.2.0

* Shiny support has been added with the `shiny_praatpicture()` function.
* The `proportion` argument no longer has to add up to 100.
* Users can now specify the location of a `.TextGrid` file when plotting
annotations.

# praatpicture 1.1.4

* It's now possible to change font sizes, text adjustment, alignment, etc. 
when using the `annotate` argument

# praatpicture 1.1.3

* Fixed bug that made it impossible to control which channel to highlight
when plotting multiple channels and other frames

# praatpicture 1.1.2 

* Added option to add a vector of strings to `wave_color`, for plotting 
different channels using different colors.

# praatpicture 1.1.1

* Fixed bug that made it impossible to control the lower frequency range of
spectrograms.

# praatpicture 1.1.0

* Options added to draw straight lines (vertical, horizontal, or otherwise) on 
plot components with the `draw_lines` argument. This replaces the 
`formant_dottedLines` argument, but doesn't otherwise change the look of the
default formant plot.
* Background coloring can now be added to lines and speckles when any of the
`*_plotOnSpec` options are `TRUE`. This is done by adding a vector of two 
strings to the `*_color` argument, e.g. `pitch_color=c('blue','lightblue')` for
a blue pitch contour on a light blue background. For `formant_color`, which 
already allowed a vector of multiple strings, this creates a range of options: 
If you pass just two strings but are plotting >2 formants, e.g. 
`c('black','white')` all formants will be plotted in black on a white background.
If you pass N formants + 1 string, the final color will be used as background
for all formants. If you pass double the number of formants, the second half
will be used for individual background colors. 
* Option added for both drawing and speckling formants and pitch in a single
plot. Before `*_plotType` had to be either `'draw'` or `'speckle'`, now also
allows `c('draw','speckle')`. 

# praatpicture 1.0.0

* On CRAN!

# praatpicture 0.6.1

* Initial CRAN submission.
