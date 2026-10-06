# facereaderconverter 0.4.0

## Breaking changes

* `extract_subject_id_metadata()` and `extract_id_subject_metadata()` no longer default to a four-digit ID and `mum`/`teen` subject taken from the export filename. By default `id` is now the media filename recorded in the export's metadata and `subject` is the export filename, both without extensions. Pass `id_pattern`/`subject_pattern` to restore regex extraction.
* `convertFRFiles()`, `convertFRExcelFiles()`, `convertFRDirectory()` and `loadFRfile()` now add `id` and `subject` columns to TXT/XLSX data by default, using the same media and export filenames. CSV inputs never get an inferred `id` and only get an inferred `subject` when `subject_pattern` is supplied. Output CSV filenames are unchanged unless `id`, `subject`, `id_pattern` or `subject_pattern` is supplied.

## New features

* `convert_directory_to_episodes()` combines detailed FaceReader TXT/XLSX exports into one in-memory episode result and `.RDa` file, using a shared FPS from export metadata. IDs come from each export's media filename and subjects from the export filename, optionally narrowed with `id_pattern`/`subject_pattern` (`use_full_path = TRUE` searches full paths). Folder names are never used. Exports whose media filename matches more than one distinct ID, or that resolve to the same ID and subject pair, are rejected (RCP-371).
* `synchrony_moments_pipeline()` reports the number of videos with exported clips as `n_videos_exported` in its manifest (RCP-406).
