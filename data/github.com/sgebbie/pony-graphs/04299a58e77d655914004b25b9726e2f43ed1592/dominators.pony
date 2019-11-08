use "collections"

/*
  // see A Simple, Fast Dominance Algorithm by Keith D. Cooper, Timothy J. Harvey, and Ken Kennedy
	// http://www.hipersoft.rice.edu/grads/publications/dom14.pdf

	for all nodes, b /* initialize the dominators array */
		doms[b] ← Undefined
	doms[start node] ← start node
	Changed ← true
	while (Changed)
		Changed ← false
		for all nodes, b, in reverse postorder (except start node)
			new idom ← first (processed) predecessor of b /* (pick one) */
			for all other predecessors, p, of b
				if doms[p] ≠ Undefined /* i.e., if doms[p] already calculated */
					new idom ← intersect(p, new idom)
			if doms[b] ≠ new idom
				doms[b] ← new idom
				Changed ← true

	function intersect(b1, b2) returns node
		finger1 ← b1
		finger2 ← b2
		while (finger1 ≠ finger2)
			while (finger1 < finger2)
				finger1 = doms[finger1]
			while (finger2 < finger1)
				finger2 = doms[finger2]
		return finger1
 */

primitive Dominators

	fun gendom(predecessors_by_postorder: Array[Array[USize]] box): Array[USize] ? =>
		"""
		Calculate the dominator tree for a graph based on post order predecessors.

		Index `i` contains the predecessors of node `i`
		Nodes are numbered in post order i.e.:
		- node `0` is the first leaf node reached,
		- and node `n-1` is the root node where `n` is the total number of nodes.

		In order to traverse the nodes in reverse order we then simply start from n-1.

		The function may throw an error if the predecessors are inconsistent.
		"""

		let node_count: USize = predecessors_by_postorder.size()

		// initialize the dominators array to undefined
		// we use max_value rather than (USize | None) in order to avoid copying the array in order to return Array[USize]
		let undef: USize = USize.max_value()
		let doms: Array[USize] = Array[USize].init(undef, node_count)

		// initialise the dominators for the start node
		let start_node: USize = node_count - 1
		doms(start_node)? = start_node

		// build the dominator tree
		var pass: USize = 0
		var changed: Bool = true
		while changed do
			changed = false
			// Traverse nodes in reverse postorder, which means starting at node node_count and working backwards
			var b: USize = node_count
			repeat
				// simple move to the next reverse postorder index
				b = b - 1

				// skip processing of the start node
				if b == start_node then continue end

				let predecessors: Array[USize] box = predecessors_by_postorder(b)?
				// Now pick any predecessor that has already been processed in this change iteration.
				// Because b is following a reverse postorder there must be at least one predecessor that was already visited.
				// But this does not imply that all predecessors have already been visited, so find one with a greater index
				// value than the node, b, we are currently considering.
				// (we can improve the performance by requiring that predecessors are reverse sorted - and then pick the first
				// one)
				let predecessors_count = predecessors.size()
				var k: USize = 0
				var new_idom: USize = 0
				while k < predecessors_count do
					new_idom = predecessors(k)?
					if new_idom > b then break end // yay, found a preprocessed predecessor
					k = k + 1
				end

				// for all other predecessors, p, of b
				var j: USize = 0
				while j < predecessors.size() do
					if j == k then j = j + 1; continue end // skip the predecessor that was used when initialising new_idom
					let p: USize = predecessors(j)?
					let dp = doms(p)?
					if (dp != undef) then
						// i.e., if doms[p] already calculated
						new_idom = _intersect(doms, p, new_idom, undef)?
					end

					j = j + 1
				end
				if doms(b)? != new_idom then
					doms(b)? = new_idom
					changed = true
				end

			until b == 0 end // use repeat-until to avoid accidental wrap-around on 0-1 = USize.max_value()
		end
		doms

	fun _intersect(doms: Array[USize], b1: USize, b2: USize, undef: USize): USize ? =>
		var finger1 = b1
		var finger2 = b2
		while (finger1 != finger2) do
			while (finger1 < finger2) do
				finger1 = doms(finger1)?
				if finger1 == undef then error end // stop loop - probably bad predecessor data
			end
			while (finger2 < finger1) do
				finger2 = doms(finger2)?
				if finger2 == undef then error end // stop loop - probably bad predecessor data
			end
		end
		finger1
