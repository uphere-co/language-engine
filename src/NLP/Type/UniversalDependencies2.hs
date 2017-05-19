module NLP.Type.UniversalDependencies2 where

data UniversalPOSTag = Open OpenClassWord
                     | Closed ClosedClassWord
                     | Other OtherClassWord
                     deriving (Show,Eq,Ord)


data OpenClassWord = ADJ   -- adjective
                   | ADV   -- adverb
                   | INTJ  -- interjection
                   | NOUN  -- noun 
                   | PROPN -- proper noun
                   | VERB  -- verb
                   deriving (Show,Eq,Ord)

data ClosedClassWord = ADP   -- adposition
                     | AUX   -- auxiliary
                     | CCONJ -- coordinating conjuction
                     | DET   -- determiner
                     | NUM   -- numeral
                     | PART  -- particle
                     | PRON  -- pronoun
                     | SCONJ -- subordinating conjunction
                     deriving (Show,Eq,Ord)

data OtherClassWord = PUNCT -- punctuation
                    | SYM   -- symbol
                    | X     -- other
                    deriving (Show,Eq,Ord)

data UniversalFeature = Lexical LexicalFeature
                      | Inflectional InflectionalFeature
                      deriving (Show,Eq,Ord)

data LexicalFeature = PronType  -- pronominal type
                    | NumType   -- numeral type
                    | Poss      -- possessive
                    | Reflex    -- reflexive
                    | Foreign   -- is this a foreign word?
                    | Abbr      -- abbreviation
                    deriving (Show,Eq,Ord)

data InfletionalFeature = Nominal NominalFeature
                        | Verbal VerbalFeature
                        deriving (Show,Eq,Ord)
 
data NominalFeature = Gender    -- gender
                    | Animacy   -- animacy
                    | Number    -- number
                    | Case      -- case
                    | Definite  -- definiteness or state
                    | Degree    -- degree of comparison
                    deriving (Show,Eq,Ord)
 
data VerbalFeature = VerbForm   -- form of verb or deverbative
                   | Mood       -- mood
                   | Tense      -- tense
                   | Aspect     -- aspect
                   | Voice      -- voice
                   | Evident    -- evidentiality
                   | Polarity   -- polarity
                   | Person     -- person
                   | Polite     -- politeness
                   deriving (Show,Eq,Ord)

