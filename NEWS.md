# facereaderconverter 0.2.5

* loadFRfile() now imports FaceReader TXT and XLSX files directly into memory by dispatching to the appropriate importer based on file extension.
* delta() is now exported as an alias of add_delta_column().
* reaction_rate() gains `minimum_threshold` filtering and the new `reaction_rate_by_episode()` helper for episode-level inspection.
