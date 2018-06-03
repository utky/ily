# ily

なんかそもそもおのれの問題を独自構文でモデル化できるのか本当に不安になってきた。

## Semantic models

Plot is represented by [DAG](https://hackage.haskell.org/package/containers-0.5.11.0/docs/Data-Graph.html).

```haskell

type Graph

type Vertex = Event ID

type Edge   = (Event ID, Event ID)
```

### Event

```haskell

-- | Database compatible identifier
type EID = UUID

hash :: EID -> Int

eid :: Event -> EID

summary :: Event -> Text

description :: Event -> Text

scenes :: Event -> [Scene]


subgraph :: Event -> Graph
```

### Scene

```haskell
text :: Scene -> Text

view :: Scene -> Actor

presence :: Scene -> [Actor]

place :: Scene -> Place

timestamp :: Scene -> Time
```

# TODO

## 1. Parser

- [-] Dec: Deriving form for function definition.
- [-] Exp: List literal
- [-] Exp: List constructor
- [-] Exp: Tuple literal
- [-] Pat: List pattern match
- [-] Pat: Tuple pattern match

## 2. Renaming

- [ ] Change data decleration to introduce Cofree
- [ ] Save token position in Cofree
- [ ] Underiving
- [ ] Indexing variable

## 3. Static semantics (Type inference)


## 4. Simplify

## 5. Dynamic semantics

