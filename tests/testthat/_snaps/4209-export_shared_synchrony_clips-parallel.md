# clip export validates core counts

    Code
      export_shared_synchrony_clips(coded_data, shared, c(`1` = video), cores = -1L)
    Condition
      Error:
      ! `cores` must be a non-negative whole number.

