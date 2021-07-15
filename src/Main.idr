module Main

import System.File
import Data.String.Extra

record HtmlAttributes where
    constructor MkHtmlAttributes
    simpleAttributes : List String
    onAttributes : List String
    onAttributesWithVal : List String

simpleAtrNameToCode : String -> String
simpleAtrNameToCode atr = 
"""
export
\{atr} : String -> Attribute event
\{atr} = ("\{atr}" =:)
"""

onAtrNameToCode : String -> String
onAtrNameToCode atr = 
"""
export
\{atr} : event -> Attribute event
\{atr} = eventAttribute "\{atr}" $ MkCmd ($ event)
"""

onAtrNameToCodeWithTargetVal : String -> String
onAtrNameToCodeWithTargetVal atr = 
"""
export
\{atr} : (String -> event) -> Attribute event
\{atr} = stringEvent "\{atr}"
"""

atrsToCode : HtmlAttributes -> String
atrsToCode allAtrs = 
    join "\n\n"
        [ join "\n\n" $
            map (\atrs => simpleAtrNameToCode atrs) allAtrs.simpleAttributes
        , join "\n\n" $
            map (\atrs => onAtrNameToCode atrs) allAtrs.onAttributes
        , join "\n\n" $
            map (\atrs => onAtrNameToCodeWithTargetVal atrs) allAtrs.onAttributesWithVal
        ]

allHtmlAttributes : HtmlAttributes
allHtmlAttributes = 
    MkHtmlAttributes 
        { simpleAttributes 
            = 
                [ "style"
                , "id"
                , "className"
                ]
        , onAttributes 
            = 
                [ "onClick"
                ]
        , onAttributesWithVal 
            = 
                ["onInput"
                ]
        }


generateAtrs : HtmlAttributes -> IO ()
generateAtrs allAtrs = ignore $ writeFile "./testFile" $ atrsToCode allAtrs

main : IO ()
main = generateAtrs allHtmlAttributes