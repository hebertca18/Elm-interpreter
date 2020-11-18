
type alias NumC = {n : number}
type alias BoolC = {b : Boolean}
type alias StringC = {s : String}
type alias IfC = {test : ExprC, t : ExprC, f : ExprC}
type alias InvalidInput = {message : String}
type alias NonBooleanTestValue = {message : String}
type alias IdC = {s : String}
type alias LamC = {params : List String, body : ExprC}
type alias AppC = {fun : ExprC, args : List ExprC}
type ExprC = NumC | BoolC | StringC | IdC | LamC | InvalidInput | NonBooleanTestValue

type alias CloV = {arg : List String, body : ExprC, env : Env}

type alias PrimopV = {op : String}
type alias NumV = {n : number}
type alias BoolV = {b : Boolean}
type alias StringV = {s : String}
type alias NonExistentBinding = {message : String}
type Value = CloV | PrimopV | NumV | BoolV | StringV | NonExistentBinding

type alias Binding = {name : String, val : Value }
type Env = List Maybe Binding

type Maybe a = Just a | Nothing

--topInterp : List String -> String
topInterp s = 
    let 
        parsed = parse s 
        val = interp parsed
    in
        serialize val

topEnv : Env
topEnv = [Just Binding "+" PrimopV "+", 
        Just Binding "-" PrimopV "-",
        Just Binding "/" PrimopV "/",
        Just Binding "*" PrimopV "*",
        Just Binding "<=" PrimopV "<=",
        Just Binding "equal?" PrimopV "equal?",
        Just Binding "true" BoolV True,
        Just Binding "false" BoolV False]

interp : ExprC -> Env -> Value
interp e env = 
    case e of
        NumC n -> NumV n
        StringC s -> StringV s
        BoolC b -> boolV b
        IfC test t f ->
           let
                testVal = interp test env 
            in
                case testVal of
                    BoolC b ->
                        if b then
                            interp t env
                        else
                            interp f env
                    _ -> NonBooleanTestValue ("Non Boolean Value: " ++ serialize testVal)
        LamC params body -> CloV params body env
        AppC fun args -> 
            let 
                funval = interp fun env
            in
                case funval of
                    CloV arg body cenv ->
                        if arg.length == args.length then
                            let 
                                newEnv = bindArguments arg args env
                            in
                                interp body newEnv
                        else 
                            InvalidInput ("Incorrect number of arguments for" ++ serialize fun)
                    PrimopV op -> handlePrimop op args env
                    _ -> InvalidInput ("First argument of AppC must be a closure or Primop" ++ serialize fun)
        IdC s -> lookup env s


serialize : Value -> String
serialize val = 
    case val of
        NumV n -> String.fromInt(n)
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

get : Int -> List a -> Maybe a
get nth list =
    list
        |> List.drop (nth - 1)
        |> List.head
        
handlePrimop : String -> List ExprC -> Env -> Value
handlePrimop op args env = 
    case op of
        "+" -> if args.length == 2 then
                let
                    left = interp args.head env
                    right = interp args.reverse.head env
                in
                    case left of
                       NumV l -> case right of
                            NumV r -> l + r
                            _ -> InvalidInput (right ++ " must evaluate to a number for +")
                        _ -> InvalidInput (left ++ " must evaluate to a number for +")
                        
lookup : Env -> String -> Value
lookup env s =
    let
        bind = env.head
    in
        case bind of
           Just Binding name val ->
            if bind.name == s then
                bind.val
            else
                lookup env.tail s
           Nothing -> NonExistentBinding ("No Binding exists for: " ++ s)

bindArguments : List String -> List ExprC -> Env -> Env
bindArguments listS listE env = 
    let
        e = listE.head
        s = listS.head
    in
        case s of
            Just name ->
                let
                    newEnv = ((Binding name e) :: env) 
                in
                    bindArguments listS.tail listE.tail newEnv
            Nothing ->
                env

                        