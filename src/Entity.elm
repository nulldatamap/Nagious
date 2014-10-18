module Entity where

import Render (CanvasLayer)

import Utils (search)
import Error (raise)

-- Component implementation
-- a: the implementation
-- b: the container ( world )
data ComponentI a b = ComponentI (a -> Entity a b -> b -> b) 

type EntityId = Int

type Entity a b = { impl : ComponentI a b, components : [a], id : EntityId }

getComponentMaybe : (a -> Bool) -> Entity a b -> Maybe a
getComponentMaybe pred ent =
  search pred ent.components

getComponent : (a -> Bool) -> Entity a b -> a
getComponent pred ent =
  case getComponentMaybe pred ent of
    Nothing -> raise <| "Tried to access component which isn't present in the given entity: "
                        ++ show ent
    Just r -> r

hasComponent : (a -> Bool) -> Entity a b -> Bool
hasComponent pred ent =
  case search pred ent.components of
    Nothing -> False
    _       -> True

modifyComponent : (a -> Bool) -> (a -> a) -> Entity a b -> Entity a b
modifyComponent pred func ent =
  { ent | components <- map (\com -> if pred com
                                        then func com
                                        else com)
                            ent.components }

updateComponent : a -> Entity a b -> b -> b
updateComponent com ent wrld =
  let (ComponentI upd) = ent.impl
  in upd com ent wrld
