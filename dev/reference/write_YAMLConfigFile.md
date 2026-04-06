# Write Auto Generated YAML BayLum Configuration File to the Disc

This little function helps to auto-generate a the BayLum YAML
configuration file or a [list](https://rdrr.io/r/base/list.html) that
can be passed to
[create_DataFile](https://crp2a.github.io/BayLum/dev/reference/create_DataFile.md).
The YAML file itself can be modified in any text editor. The allowed
parameters are extracted from the reference YAML file

## Usage

``` r
write_YAMLConfigFile(output_file = NULL, ...)
```

## Arguments

- output_file:

  [character](https://rdrr.io/r/base/character.html) (*with default*):
  valid file path of the output file

- ...:

  parameters to be preset in the YAML file (run `write_YAMLConfigFile()`
  to see allowed parameters) The parameter `sample` is special, because
  it can be provided as a
  [character](https://rdrr.io/r/base/character.html) vector of any
  length. The number of elements in the vector (sample names) are then
  used to multiply the records in the configuration file.

## Value

The function has two output modes:

- \(1\) `output_file = <file_path>`: Writes a YAML into the specified
  path and returns this path.

- \(2\) `output_file = NULL`: Returns a list of allowed function
  parameters that can be passed to the function **and** it returns a
  list that can be used a passed on to
  [create_DataFile](https://crp2a.github.io/BayLum/dev/reference/create_DataFile.md).

## Function version

0.1.0

## See also

[create_DataFile](https://crp2a.github.io/BayLum/dev/reference/create_DataFile.md),
[yaml::read_yaml](https://yaml.r-lib.org/reference/read_yaml.html),
[Luminescence::read_BIN2R](https://r-lum.github.io/Luminescence/reference/read_BIN2R.html),
[Luminescence::read_XSYG2R](https://r-lum.github.io/Luminescence/reference/read_XSYG2R.html)

## Author

Sebastian Kreutzer, Institute of Geography, Ruprecht-Karl University of
Heidelberg (Germany)

## Examples

``` r
## generate list
write_YAMLConfigFile(
sample = c("samp1", "samp2"),
settings.rules.endTest = 10)
#> ── Allowed function parameters (start) ─────────────────────────────────────────
#> sample
#> files
#> settings.dose_points
#> settings.dose_source.value
#> settings.dose_source.error
#> settings.dose_env.value
#> settings.dose_env.error
#> settings.rules.beginSignal
#> settings.rules.endSignal
#> settings.rules.beginBackground
#> settings.rules.endBackground
#> settings.rules.beginTest
#> settings.rules.endTest
#> settings.rules.beginTestBackground
#> settings.rules.endTestBackground
#> settings.rules.inflatePercent
#> settings.rules.nbOfLastCycleToRemove
#> ── Allowed function parameters (end) ───────────────────────────────────────────

## generate file (here written in tempdir)
write_YAMLConfigFile(
 output_file = tempfile("configuration.yml"),
 sample = c("samp1", "samp2"),
 settings.rules.endTest = 10)
#> [1] "/tmp/RtmpuRrOip/configuration.yml1c8255f045da.yml"
```
