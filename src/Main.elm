module Main exposing  (..)
import Maybe exposing (withDefault)
import String exposing (String)

{--type alias NumC = {n : Float}
type alias BoolC = {b : Bool}
type alias StringC = {s : String}
type alias IfC = {test : ExprC, t : ExprC, f : ExprC}
type alias InvalidInput = {message : String}
type alias NonBooleanTestValue = {message : String}
type alias IdC = {s : String}
type alias LamC = {params : List String, body : ExprC}
type alias AppC = {fun : ExprC, args : List ExprC} --}
type ExprC = NumC Float
    | BoolC Bool
    | StringC String
    | IdC String
    | LamC (List String) ExprC
    | IfC ExprC ExprC ExprC
    | AppC ExprC (List ExprC)
    --| InvalidInputExpr String
    --| NonBooleanTestValue String



{-- type alias CloV = {arg : List String, body : ExprC, env : Env}
type alias PrimopV = {op : String}
type alias NumV = {n : Float}
type alias BoolV = {b : Bool}
type alias StringV = {s : String}
type alias NonExistentBinding = {message : String} --}
type Value = CloV {arg : List String, body : ExprC, env : Env}
    | PrimopV String
    | NumV Float
    | BoolV Bool
    | StringV String
    | NonExistentBinding String
    | NonBooleanTestInput String
    | NonBooleanTestValue String
    | InvalidInput String

type alias Binding = {name : String, val : Value }
type alias Env = List Binding
--type alias Env = List (Maybe Binding)
-- type alias Env = {env : Maybe Binding}

type Maybe a = Just a | Nothing

--topInterp : List String -> String
{--topInterp s = 
    let 
        parsed = parse s 
        val = interp parsed
    in
        serialize val--}

topEnv : Env
topEnv = [(Binding "+" (PrimopV "+")), 
        (Binding "-" (PrimopV "-")),
        (Binding "/" (PrimopV "/")),
        (Binding "*" (PrimopV "*")),
        (Binding "<=" (PrimopV "<=")),
        (Binding "equal?" (PrimopV "equal?")),
        (Binding "true" (BoolV True)),
        (Binding "false" (BoolV False))]

interp : ExprC -> Env -> Value
interp e env = 
    case e of
        NumC n -> NumV n
        StringC s -> StringV s
        BoolC b -> BoolV b
        IfC test t f ->
           let
                testVal = interp test env 
            in
                case testVal of
                    BoolV b ->
                        if b then
                            interp t env
                        else
                            interp f env
                    _ -> NonBooleanTestValue ("Non Boolean Value: " ++ serialize testVal)
        LamC params body -> CloV {arg = params, body = body, env = env}
        AppC fun args -> 
            let 
                funval = interp fun env
            in
                case funval of
                    CloV clo ->
                        if List.length clo.arg == List.length args then
                            let 
                                newEnv = bindArguments clo.arg args env
                            in
                                interp clo.body newEnv
                        else 
                            InvalidInput ("Incorrect number of arguments for" ++ serialize funval)
                    PrimopV op -> handlePrimop op args env
                    _ -> InvalidInput ("First argument of AppC must be a closure or Primop" ++ serialize funval)
        IdC s -> lookup env s


serialize : Value -> String
serialize val = 
    case val of
        NumV n -> String.fromFloat(n)
        CloV _ -> "#<procedure>"
        PrimopV _ -> "#<primop>"
        BoolV b -> if b == True then
                        "true"
                    else 
                        "false"
        StringV s -> s
        InvalidInput message -> message
        NonBooleanTestInput message -> message
        NonExistentBinding message -> message
        _ -> "ERROR: Unrecognized value."

{--get : Int -> List a -> Maybe a
get nth list =
    list
        |> List.drop (nth - 1)
        |> List.head--}
        
handlePrimop : String -> List ExprC -> Env -> Value
handlePrimop op args env = 
    case op of
        "+" -> case args of
               [a,b] -> let
                            left = interp a env
                            right = interp b env
                        in
                            case left of
                                NumV l -> case right of
                                        NumV r -> NumV (l + r)
                                        _ -> InvalidInput ((serialize right) ++ " must evaluate to a number for +")
                                _ -> InvalidInput ((serialize left) ++ " must evaluate to a number for +")
                --[] -> InvalidInput "Invalid number of arguments for a primop"
                    --InvalidInput ("Invalid number of arguments for a primop")
        _ -> InvalidInput ("Invalid primop " ++ op)
               
            {--let
                left = interp (List.head args) env
                maybeList = (List.head (List.reverse args))
            in
                case maybeList of
                    Just arg -> 
                        let 
                            right = interp arg env
                        in
                            case left of
                                NumV l -> case right of
                                        NumV r -> NumV (l + r)
                                        _ -> InvalidInput ((serialize right) ++ " must evaluate to a number for +")
                                _ -> InvalidInput ((serialize left) ++ " must evaluate to a number for +")

                    Nothing -> InvalidInput "Invalid Expression"
                    
                else
                    InvalidInput ("Invalid number of arguments for a primop")--}
                        
lookup : Env -> String -> Value
lookup env s =
    let
        bind = case env of
            f::_ -> Just f
            [] -> Nothing
        --bind = env.head
    in
        case bind of
           Just binding ->
            if binding.name == s then
                binding.val
            else
                case env of
                   x :: rest -> lookup rest s
                   [] -> NonExistentBinding ("The binding for: " ++ s ++ " does not exist.")
                 {--let
                    newEnv = List.tail env
                in
                    case newEnv of
                        Just e -> 
                            case e of
                               Just x -> lookup x s
                               Nothing -> NonExistentBinding ("The binding for: " ++ s ++ " does not exist.")
                        Nothing -> NonExistentBinding ("The binding for: " ++ s ++ " does not exist.")--}
           Nothing -> NonExistentBinding ("No Binding exists for: " ++ s)

bindArguments : List String -> List ExprC -> Env -> Env
bindArguments listS listE env = 
    let
        e = case listE of 
            x :: xs -> x
            [] -> Nothing
        s = case listS of 
            x :: xs -> x
    in
        let
            newEnv = ((Binding s (interp e env)) :: env) 
            --newEnv = ((Binding s (interp e env)) :: env) 
        in
            case listE of
                x :: restE -> case listS of
                                y :: restS -> bindArguments restS restE newEnv
                [] -> env
            --bindArguments (List.tail listS) (List.tail listE) newEnv
        {-- case s of
            Just name ->
                let
                    newEnv = ((Binding name e) :: env) 
                in
                    bindArguments (List.tail listS) (List.tail listE) newEnv
            Nothing ->
                env --}

                        