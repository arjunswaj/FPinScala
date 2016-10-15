import com.asb.ds.{Branch, Leaf, Tree}

val tree = Branch(Leaf(5), Branch(Leaf(6), Leaf(7)))

Tree.size(tree)
Tree.maximum(tree)
Tree.depth(tree)

Tree.map(tree)(a => a * a)

Tree.size2(tree)
Tree.maximum2(tree)
Tree.depth2(tree)
Tree.map2(tree)(a => a + 5)