# Function Summary Table

| Package | Function | Purpose | Example |
|----------|-----------|----------|----------|
| **evapoRe** | `download_data()` | Downloads standardized evapotranspiration datasets (e.g., **GLEAM**, **CAMELE**, **MSWX**) directly from the Zenodo repository and saves them locally for analysis. | `download_data("GLEAM", path = "data/")` |
| **evapoRe** | `detect_exeve()` | Detects **Extreme Evaporation Events (ExEvEs)** using user-defined percentile and persistence thresholds (e.g., 95th percentile for extremes, 0.8 for persistence). | `detect_exeve(x, 0.95, 0.8)` |
| **evapoRe** | `infoNC()` | Prints metadata from a NetCDF file, including variable names, units, spatial resolution, CRS, and temporal coverage. | `infoNC(gleam)` |
| **twc** | `subset_data()` | Subsets raster data by longitude-latitude box and year range, keeping only the region and time period of interest. | `subset_data(x, box = c(-10, 40, 35, 70), yrs = c(1981, 2022))` |
| **twc** | `crop_data()` | Crops raster data to a polygon boundary (e.g., a country shapefile) to extract spatially-masked subsets. | `crop_data(x, "gadm41_ESP_0.shp")` |
| **twc** | `tabular()` | Converts raster time-series data into a tabular (`data.table`) structure for further statistical or graphical analysis. | `tabular(x)` |
| **evapoRe** | `fldmean()` | Computes the area-weighted mean (field mean) across all raster cells for each time step, producing a regional time series. | `fldmean(x)` |
| **evapoRe** | `plot_map()` | Displays a spatial map of a selected raster layer (e.g., one monthly slice of ET or PET data). | `plot_map(x[[18]])` |
| **evapoRe** | `plot_line()` | Plots a time-series line graph for variables such as ET or PET. | `plot_line(ts, var = "ET")` |
| **evapoRe** | `plot_heatmap()` | Creates a month-year heatmap showing temporal patterns and anomalies in the time series. | `plot_heatmap(ts)` |
| **evapoRe** | `plot_box()` | Produces boxplots of ET or PET by month to reveal seasonal distributions. | `plot_box(ts, var = "ET")` |
| **evapoRe** | `plot_density()` | Draws probability-density curves illustrating the overall distribution of ET or PET values. | `plot_density(ts, var = "ET")` |
| **evapoRe** | `plot_summary()` | Generates a combined visualization panel (line + box + heatmap + density) summarizing spatiotemporal variability at once. | `plot_summary(ts, var = "Evapotranspiration")` |

*Notes*
- `evapoRe` provides tools for downloading, processing, summarizing, and visualizing evapotranspiration datasets.  
- `twc` (Time–Weather Climate) offers spatial and temporal operations that complement `evapoRe`.  
