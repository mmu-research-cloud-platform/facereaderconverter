# facereaderconverter development version

- `export_shared_synchrony_clips()` encodes independent clips concurrently by default, with `cores = 1` for serial encoding; `synchrony_moments_pipeline()` forwards its `cores` setting to clip export (RCP-392).
