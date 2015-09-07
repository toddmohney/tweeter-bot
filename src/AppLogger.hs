module AppLogger where
  log :: FilePath -> String -> IO ()
  log filePath str = appendFile filePath $ str ++ "\n"
