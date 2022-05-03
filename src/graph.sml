signature GRAPH = sig

type node
type 'a graph

(*
   Create a new node in the graph
   This operation is not a pure function.
*)
val empty   : unit -> 'a graph
val newNode : 'a graph -> 'a  -> node

(* If the data structure was supposed to be persistent the definition
   of new will not be of this form

   addNode : graph -> graph * node
   addEdge : graph -> node * node -> graph
*)


val addEdge : 'a graph -> (node * node) -> unit

(* addEdge (a,b) should add and edge starting from a to b *)

val succ    : 'a graph -> node -> node list
val pred    : 'a graph -> node -> node list
val label   : 'a graph -> node -> 'a

val clear   : 'a graph -> unit
val all     : 'a graph -> node list

(* you might want functions that go over all the nodes

maps, folds etc
*)

end

structure Graph :> GRAPH = struct
    type node = word

    structure NodeHashKey : HASH_KEY = struct
	    type hash_key = node
	    fun  hashVal w = w
	    fun  sameKey (w1,w2) = w1 = w2
    end

    structure NodeSet = HashSetFn (NodeHashKey)

    type nodeSet = NodeSet.set

    type 'a graph = { 
        labels : (node, 'a)  HashTable.hash_table,
		(* edges *)
		successors      : (node, nodeSet) HashTable.hash_table,
		predecessors    : (node, nodeSet) HashTable.hash_table,
		nextNode        : node ref
	}

    fun empty () = { 
        labels          = HashTable.mkTable (NodeHashKey.hashVal, NodeHashKey.sameKey) (0, Fail "Not found"),
        successors      = HashTable.mkTable (NodeHashKey.hashVal, NodeHashKey.sameKey) (0, Fail "Not found"),
        predecessors    = HashTable.mkTable (NodeHashKey.hashVal, NodeHashKey.sameKey) (0, Fail "Not found"),
        nextNode        = ref (Word.fromInt 0)
    }

    fun newNode (g: 'a graph) a = 
                                let
                                    val nn = !(#nextNode g)
                                    val _  = HashTable.insert (#labels g) (nn, a)
                                    val _  = HashTable.insert (#successors g) (nn, NodeSet.mkEmpty 0)
                                    val _  = HashTable.insert (#predecessors g) (nn, NodeSet.mkEmpty 0)
                                    val _  = (#nextNode g) := nn + (Word.fromInt 1)
                                in 
                                    nn
                                end
    
    fun addEdge (g: 'a graph) (a1, a2) = 
                                        let 
                                            val _ = NodeSet.add((HashTable.lookup (#successors g) a1), a2)
                                            val _ = NodeSet.add((HashTable.lookup (#predecessors g) a2), a1)
                                        in
                                            ()
                                        end

    fun succ (g: 'a graph) a = NodeSet.listItems (HashTable.lookup (#successors g) a)

    fun pred (g: 'a graph) a = NodeSet.listItems (HashTable.lookup (#predecessors g) a)

    fun label (g: 'a graph) a = HashTable.lookup (#labels g) a

    fun clear (g: 'a graph) =   let
                                    val _ = HashTable.clear (#labels g)
                                    val _ = HashTable.clear (#successors g)
                                    val _ = HashTable.clear (#predecessors g)
                                    val _ = (#nextNode g) := (Word.fromInt 0)
                                in
                                    ()  
                                end

    fun all (g: 'a graph) =     let
                                    val x = HashTable.listItemsi (#labels g)
                                    fun f [] = []
                                    |   f ((a,b)::xs) = (a)::(f xs)
                                in
                                    f x
                                end 

end