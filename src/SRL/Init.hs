module SRL.Init where

initGHCi :: IO J.JVM
initGHCi = do
  clspath <- getEnv "CLASSPATH"
  J.newJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] 
