# facereaderconverter 0.3.2.2

* `convert_directory_to_episodes()` combines detailed FaceReader TXT/XLSX exports into one in-memory episode result and `.RDa` file, inheriting a shared FPS from export metadata and optionally matching ID and subject from filenames or enclosing folders; it rejects distinct IDs matching the supplied regex in an export's filename or eligible folders (RCP-371).
