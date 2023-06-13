-- type Id = String
-- type Var = (Id, Tipo)
-- data Tipo = TDouble | TInt | TString | TVoid deriving Show

-- getVars :: [(Tipo, [Id])] -> [Var]
-- getVars [] = []
-- getVars ((tipo, []) : t) = getVars(t)
-- getVars ((tipo, id : ids) : t) = (id, tipo) : getVars ((tipo, ids) : t)