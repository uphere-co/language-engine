module WikiEL.Old
  ( loadWikiData
  ) where

import           Data.Text               ( Text )
import           NLP.Type.NamedEntity    ( NamedEntityClass )
import           NLP.Type.PennTreebankII ( POSTag )
import qualified WikiEL as WEL
import           WikiEL.Type             ( EntityMention )
import           WikiEL.Run              ( classFilesG
                                         , graphFilesG
                                         , reprFileG
                                         , wikiTitleMappingFileG
                                         )


loadWikiData ::
       FilePath
    -> IO ( [(Text, NamedEntityClass,POSTag)] -> [EntityMention Text]
          , [EntityMention Text] -> [EntityMention Text])
loadWikiData dataDir = do
  edges  <- WEL.loadAndSortEdges (graphFilesG dataDir)
  uidTag <- WEL.loadFiles        (classFilesG dataDir)
  titles <- WEL.loadWikipageMapping (wikiTitleMappingFileG dataDir)
  tagger <- WEL.loadFEMtagger (reprFileG dataDir) (classFilesG dataDir)
  let entityResolve = WEL.disambiguateMentions edges uidTag titles
  return (tagger,entityResolve)

