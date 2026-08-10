# API Credentials

Two functions in this package need API credentials, passed in as parsed YAML:

- **`hv_auth()`** authenticates against the HydroVu API. Needs a `client` and
  `secret` key — your HydroVu API client ID and secret. Create these from your
  HydroVu account's People & Permissions page, under "Manage API Access
  Credentials."
- **`load_mWater()`** pulls field notes from mWater. Needs a `url` key — your
  mWater deployment's CSV data export URL, found in your mWater account's
  data export settings.

## Usage

1. Copy `HydroVuCredsTemplate.yml` and/or `mWaterCredsTemplate.yml` to a
   location of your choosing outside this package directory (e.g. a `creds/`
   folder in your own project).
2. Fill in your real values in the copy.
3. Load the file and pass it to the relevant function:

```r
hv_creds <- yaml::read_yaml("path/to/your/HydroVuCreds.yml")
hv_auth(client_id = as.character(hv_creds["client"]),
        client_secret = as.character(hv_creds["secret"]))

mWater_creds <- yaml::read_yaml("path/to/your/mWaterCreds.yml")
load_mWater(creds = mWater_creds)
```

## Keep your filled-in credentials out of version control

Never commit a file with real values — only the `*Template.yml` files in this
directory should ever be tracked. If you copy your filled-in file into a
project that has this package's `.gitignore`/`.Rbuildignore` conventions, put
it under a root-level `creds/` folder, which is already excluded. Otherwise,
add your credentials path to your own project's `.gitignore` before filling
it in.
