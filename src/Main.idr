module Main

import System.File
import Data.String.Extra

record SimpleHtmlAtr where
    constructor MkSimpleHtmlAtr
    name : String
    type : String

record HtmlAttributes where
    constructor MkHtmlAttributes
    simpleAttributes : List SimpleHtmlAtr
    onAttributes : List String
    onAttributesWithVal : List String

simpleAtrNameToCode : SimpleHtmlAtr -> String
simpleAtrNameToCode simpleAtr =
"""
export
\{simpleAtr.name} : \{simpleAtr.type} -> Attribute event
\{simpleAtr.name} = ("\{simpleAtr.name}" =:)
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
                [ -- GLOBAL ATRS!
                  MkSimpleHtmlAtr { name = "className", type = "String" }
                , MkSimpleHtmlAtr { name = "accessKey", type = "Char" }
                , MkSimpleHtmlAtr { name = "contenteditable", type = "Bool" }
                , MkSimpleHtmlAtr { name = "dir", type = "String" } -- Need Custom Type
                , MkSimpleHtmlAtr { name = "draggable", type = "Bool" }
                , MkSimpleHtmlAtr { name = "hidden", type = "Bool" }
                , MkSimpleHtmlAtr { name = "id", type = "String" }
                , MkSimpleHtmlAtr { name = "lang", type = "String" } -- Need Custom Type
                , MkSimpleHtmlAtr { name = "spellCheck", type = "Bool" }
                , MkSimpleHtmlAtr { name = "style", type = "Object" }
                , MkSimpleHtmlAtr { name = "tabIndex", type = "Int" }
                , MkSimpleHtmlAtr { name = "title", type = "String" }
                -- ATRS!
                , MkSimpleHtmlAtr { name = "accept", type = "String" } -- Need Custom Type
                , MkSimpleHtmlAtr { name = "charset", type = "String" } -- Need Custom Type
                ]

        , onAttributes
            =
                [ "onClick"
                , "onBlur"
                , "onFocus" 
                , "onReset"
                , "onSubmit"
                , "onLoad"
                , "onDoubleClick"
                , "onDrag"
                , "onDragEnd"
                , "onDragEnter"
                , "onDragExit"
                , "onDragLeave"
                , "onDragOver"
                , "onDragStart"
                , "onMouseDown"
                , "onMouseEnter"
                , "onMouseLeave"
                , "onMouseMove"
                , "onMouseOut"
                , "onMouseOver"
                , "onMouseUp"
                , "onPointerDown" 
                , "onPointerMove" 
                , "onPointerUp" 
                , "onPointerCancel" 
                , "onGotPointerCapture"
                , "onLostPointerCapture" 
                , "onPointerEnter" 
                , "onPointerLeave" 
                , "onPointerOver" 
                , "onPointerOut"
                , "onSelect"
                , "onTouchCancel" 
                , "onTouchEnd" 
                , "onTouchMove" 
                , "onTouchStart"
                , "onScroll"
                , "onWheel"
                , "onAfterPrint"
                , "onBeforePrint"
                , "onBeforeUnload"
                , "onMessage"
                , "onOffline"
                , "onOnline"
                , "onPageHide"
                , "onPageShow"
                , "onResize"
                , "onUnload"
                ]
        , onAttributesWithVal
            = 
                [ "onInput"
                , "onChange"
                , "onInvalid"
                , "onContextMenu"
                , "onDrop"
                , "onPopState"
                , "onStorage"
                ]
        }

qustionableAtrs : List String
qustionableAtrs = 
    [ "onError"
    , "onHashChange"
    , "data-*"
    , "translate" -- No Browser support
    ]


generateAtrs : HtmlAttributes -> IO ()
generateAtrs allAtrs = ignore $ writeFile "./generatedAtrs" $ atrsToCode allAtrs

main : IO ()
main = generateAtrs allHtmlAttributes