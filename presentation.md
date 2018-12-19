# Haskell Language and Lamba Calculus
What is Haskell?
* Completely functional language
* Static typing with type inference 
* Can do anything that an imperative language can do
    * But like any language, it's better at some things than others
* Absolutely nothing like you've ever seen before
    * You can decide if that's good or bad

## Types
As mentioned before, Haskell is  Statically typed like C++ or Java, but the way that types are used in Haskell is very different than in any C-ish language.

For example, here is a type declaration in C++ :

```c++ 
int num;
float num_2 = 9;
int *list = new int[30];

int squared (int a )
{
    return a*a;
}
```
In haskell, type declarations involve the `::` operator, which should be read as "is of type".

```haskell
num :: Int

num_2 = 9.0

list :: [Int]
list = []

squared :: Int -> Int
squared n = n*n
```
If functions are the heart of haskell, types are... the aorta? In any case, they are very very important. So important that in haskell they deserve their own line, and the typing system is more descriptive and important in Haskell than in imperative languages. To truly understand why, first we have to look at a function with multiple parameters.

```c++
float ave (float a, float b){
    return (float a + float b)/2;
}
```

```haskell
ave :: Float -> Float -> Float
ave a b = (a + b )/2
```
In Haskell, as in lamba calculus, EVERYTHING  is an expression (except for the things that aren't... which are few and far between so I won't be going into them now).

```haskell
map :: (a->b) -> [a] -> [b]
map f [] = []
map f a:as = (f a) : $ map f as
```
## Lambda Terms
They're kinda like lambda calculus... but only slightly more so than Haskell as a whole in my fair opinion.

Their precense, however, make Haskell much more Lambda-y... since they re-enforce the EVERYTHING is an expression principle.

```haskell
\x -> x+1
```
Why would we want this? In theory, we could do something like:
```haskell
func x = (\a ->(a+1)) x
```
But thats overkill and a little stupid. So what's the point? Observe:
```haskell
map ((\x y -> (x + y)/2) 80) [20,30,40,50,60,70,80,90]
```
versus
```haskell
ave :: Double -> Double
ave a b = (a + b)/2
map (ave 80) [20,30,40,50,60,70,80,90]
```

## Type Declarations:
In Haskell, we are free to define our own Types

```haskell
data Bool = False | True

data Time = Date Int Int Int  | Seconds Int

data Time = Date {Day :: Int, 
                  Month :: Int,
                  Year :: Int}
```
## Method Behind the Madness
A simple and rather trivial implementation of a BST in C++
```c++
using namespace std;

template<class T>
class TreeNode
{
	public:
	
	TreeNode();
	TreeNode(T& d);//constructor given data d of type T to store
	TreeNode(int k, T& d);//constructor given key k and data d
	
	int key;//this variable determines the position of the node in the BST
	T data;//data of type T to be stored in the BST
	TreeNode* left;//left child
	TreeNode* right;//right child
	
};

/**
*Begin TreeNode Implementation
**/

template<class T>
TreeNode<T>::TreeNode()
{
	key=0;
	left=NULL;
	right=NULL;
}

template<class T>
TreeNode<T>::TreeNode(T& d)
{
	TreeNode();
	data=d;
}

template<class T>
TreeNode<T>::TreeNode(int k, T& d)
{
	key=k;
	data=d;
	left=NULL;
	right=NULL;
}


/**
*End TreeNode Implementation
**/

template<class T>
class BST
{
	public:
	
	BST();
	BST(const BST<T>& tree);//copy constructor, calls: copyTree(Treenode* node)
	~BST();
	
	/**
	*Function inserts a node with data d and key k into the BST
	*Parameters: int k = node key; T d = data of type T to be stored in node 
	**/
	void insert(int k, T d);
	
	/**
	*This function is to be used to fetch the value of the data variable of the node with key k
	*Value of data variable will be passed through the ret variable
	*Parameters: int k= key of node; T& ret= variable to return item stored in data variable of node
	*Returns: True if node with key k was found, false if else
	**/
	bool getData(int k, T& ret);
	bool getData(int k, T*& ret);
	
	/**
	*This function is to be used to fetch the node with key k
	*Parameters: int k= key of node to be fetched
	*Returns: NULL if no node with key k is found, returns the node if a node with key k is found
	**/
	TreeNode<T>* getNode(int k);
	
	
	/**
	*This function deletes the node with key k
	*Parameter: int k= key of node to be deleted
	*Returns: True if node with key k is found in tree, false if else
	**/
	bool deleteNode(int k);
	
	/**
	*Does the same things as overloaded function, but also passes the key of the successor to pass param
	*Parameters: int k= key of node to be deleted, TreeNode<T>* pass= param to pass successor node
	*Returns: True if node with key k is found in tree, false if else
	*If no successor is neccesary, pass=root, if tree is empty after deletion, pass=NULL
	**/
	bool deleteNode(int k, TreeNode<T>*& pass);
	
	/**
	*Parameter: T d= data to be searched for in tree
	*Returns: True if node containing data d was found in tree, false if else
	**/
	bool contains(T d);
	
	/**
	*Parameter: int k= key of node to be searched for in tree
	*Returns: True if node with key k was found, false if else
	**/
	bool hasKey(int k);
	

	//Returns: True if Tree contains no nodes, false if else
	bool isEmpty();
	
	//Returns: pointer to node with key of least value
	TreeNode<T>* getMin();
	
	//Returns: pointer to node with key of greatest value
	TreeNode<T>* getMax();
	
	//Returns: pointer to left child of given node
	TreeNode<T>* getLeft(TreeNode<T>* node);
	
	//Returns: pointer to right child of given node
	TreeNode<T>* getRight(TreeNode<T>* node);
	
	//Returns: pointer to root node
	TreeNode<T>* getRoot();
	
	//Prints data of given node and of all descendant nodes in order of least key to greatest
	void printTree(TreeNode<T>* node);
	
	//Prints data of entire tree in order of least key to greatest
	void printTree();
	
	
	
	
	private:
	
	/**
	*Parameter: int k= key of node to be returned, TreeNode* node= node at which to begin the search
	*Returns: pointer to node with key k if such a node is found, a null pointer if else
	**/
	TreeNode<T>* findByKey(int k, TreeNode<T>* node);
	
	/**
	*Parameter: T d= data stored in the node to be returned, TreeNode*node= node at which to begin the search
	*Returns: pointer to the node storing data d if such a node is found, a null pointer if else 
	*Note: Function should not be re-defined as an overload of findByKey in case that template type T is defined as int
	**/
	TreeNode<T>* findByData(T d, TreeNode<T>*node);
	
	/**
	*This function copies all entries of a BST starting at the given node into the BST for which the function is called
	*Parameter: root node of tree to be copied
	*Used in copy constructor
	**/
	void copyTree(TreeNode<T>* node);
	
	/**
	*This function deletes the given node and all decendants of that node
	*Parameter: Node at which to begin deleting
	*used in destructor
	**/
	void deleteAll(TreeNode<T>* node);
	
	/**
	*This function returns the successor of node d and sets the right child of the successor to the right child of d
	*Parameter: Node for which to find the successor
	**/
	TreeNode<T>* getSuccessor(TreeNode<T>*d);
	
	//Root node
	TreeNode<T>* root;
};

/**
*Begin Operator Overloads
**/

template<class T>
bool operator ==(const TreeNode<T>& t1, const TreeNode<T>& t2)
{
	if(t1->data==t2->data)
		return true;
	return false;
}

template<class T>
bool operator <(const TreeNode<T>& t1, const TreeNode<T>& t2)
{
	if(t1->data<t2->data)
		return true;
	return false;
}

template<class T>
bool operator <=(const TreeNode<T>& t1, const TreeNode<T>& t2)
{
	if(t1->data<=t2->data)
		return true;
	return false;
}

template<class T>
bool operator >(const TreeNode<T>& t1, const TreeNode<T>& t2)
{
	if(t1->data>t2->data)
		return true;
	return false;
}

template<class T>
bool operator >=(const TreeNode<T>& t1, const TreeNode<T>& t2)
{
	if(t1->data>=t2->data)
		return true;
	return false;
}

/**
*End Operator Overloads
**/


/**
*Begin BST implementation
**/

template<class T>
BST<T>::BST()
{
	root=NULL;
}

template<class T>
BST<T>::BST(const BST<T>& tree)
{
	root=NULL;
	copyTree(tree.root);
}

template<class T>
BST<T>::~BST()
{
	deleteAll(root);
}


template<class T>
void BST<T>::copyTree(TreeNode<T>* node)
{
	if(node==NULL)
		return;
	insert(node->key, node->data);
	copyTree(node->left);
	copyTree(node->right);
}

template<class T>
void BST<T>::deleteAll(TreeNode<T>* node)
{
	if(node==NULL)
		return;
	deleteAll(node->left);
	deleteAll(node->right);
	delete node;
}

template<class T>
void BST<T>::insert(int k, T d)
{
	TreeNode<T>* node= new TreeNode<T>(k, d);
	if(isEmpty())
		root=node;
	
	else{//not empty
		TreeNode<T> *curr= root;
		TreeNode<T> *parent=NULL;
		while(true)//iterate and find insertion point
		{
			parent =curr;
			
			if(k<curr->key)//go left
			{
				curr=curr->left;
				if(curr==NULL)//we found our insertion point
				{
					parent->left=node;
					break;
				}
			}
			else{
				curr=curr->right;
				if(curr==NULL)//we found our insertion point
				{
					parent->right=node;
					break;
				}
			}
		}
	}
}

template<class T>
bool BST<T>::getData(int k, T& ret)
{
	TreeNode<T>* node=findByKey( k, root);
	
	if(node==NULL)
		return false;
	
	ret=node->data;
	return true;
}

template<class T>
bool BST<T>::getData(int k, T*& ret)
{
	TreeNode<T>* node=findByKey( k, root);
	
	if(node==NULL)
	{
		return false;
	}
	ret=&node->data;
	return true;
}


template<class T>
TreeNode<T>* BST<T>::getNode(int k)
{
	TreeNode<T>* node=findByKey( k, root);
	return node;
}

template<class T>
TreeNode<T>* BST<T>::findByKey(int k, TreeNode<T>* node)
{
	if (node==NULL)
		return node;
	if(k==node->key)
		return node;
	if(k<node->key)
		return findByKey(k, node->left);
	if(k>node->key)
		return findByKey(k, node->right);
	
	else
		return NULL;
}

template<class T>
bool BST<T>::deleteNode(int k)
{
	if(root==NULL)
		return false;
	
	TreeNode<T> *curr= root;
	TreeNode<T> *parent=root;
	
	bool isLeft;
	
	while(curr->key!= k) //searching for the NextToDelete
	{
		parent = curr;
		
		if(k<curr->key)
		{
			isLeft= true;
			curr= curr->left;
		}
		else{
			isLeft=false;
			curr= curr-> right;
		}
		
		if(curr==NULL)
			return false;
	}
	//once we are here, we found the node
	
	//no children
	if(curr->left==NULL&&curr->right==NULL)
	{
		if (curr==root)
		{
			root=NULL; //null out the root 
		}
		else if(isLeft)
		{
			parent->left=NULL;
		}
		else
		{
			parent->right=NULL;
		}
		
	}
	
		//one child
		//no right child
	else if (curr->right==NULL)
	{
		if(curr==root)
			root=curr->left;
		else if (isLeft)
		{
			parent->left = curr->left;
		}
		else 
		{
			parent->right=curr->left;
		}
	}
		
	//no left child
	else if (curr->left==NULL)
	{
		if(curr==root)
			root=curr->right;
		else if (isLeft)
		{
			parent->left = curr->right;
		}
		else 
		{
			parent->right=curr->right;
		}
	}
	
	//two children
	else 
	{
		TreeNode<T> *successor=getSuccessor(curr);
		if(curr==root)
		{
			root=successor;
		}
		else if(isLeft)
			parent->left=successor;
		else
			parent->right=successor;
		
		//connect successor left child to current left chidren
		successor->left=curr->left;
	}
	delete curr;
	return true;
}

template<class T>
bool BST<T>::deleteNode(int k, TreeNode<T>*& pass)
{
	if(root==NULL)
		return false;
	
	TreeNode<T> *curr= root;
	TreeNode<T> *parent=root;
	
	bool isLeft;
	
	while(curr->key!= k) //searching for the NextToDelete
	{
		parent = curr;
		
		if(k<curr->key)
		{
			isLeft= true;
			curr= curr->left;
		}
		else{
			isLeft=false;
			curr= curr-> right;
		}
		
		if(curr==NULL)
			return false;
	}
	//once we are here, we found the node
	
	//no children
	if(curr->left==NULL&&curr->right==NULL)
	{
		if (curr==root)
		{
			root=NULL; //null out the root 
			pass=NULL;
		}
		else if(isLeft)
		{
			parent->left=NULL;
			pass=root;
		}
		else
		{
			parent->right=NULL;
			pass=root;
		}
		
	}
	
		//one child
		//no right child
	else if (curr->right==NULL)
	{
		if(curr==root)
			root=curr->left;
		else if (isLeft)
		{
			parent->left = curr->left;
		}
		else 
		{
			parent->right=curr->left;
		}
		pass=curr->left;
	}
		
	//no left child
	else if (curr->left==NULL)
	{
		if(curr==root)
			root=curr->right;
		else if (isLeft)
		{
			parent->left = curr->right;
		}
		else 
		{
			parent->right=curr->right;
		}
		pass=curr->right;
	}
	
	//two children
	else 
	{
		TreeNode<T> *successor=getSuccessor(curr);
		if(curr==root)
		{
			root=successor;
		}
		else if(isLeft)
			parent->left=successor;
		else
			parent->right=successor;
		
		//connect successor left child to current left chidren
		successor->left=curr->left;
		pass=successor;
	}
	delete curr;
	return true;
}

template<class T>
TreeNode<T>* BST<T>::getSuccessor(TreeNode<T> *d)//d is the node to be deleted
{
	TreeNode<T> *sp = d;
	TreeNode<T> *successor=d;
	TreeNode<T> *curr=d->right;
	
	//loop until we find successor
	//which will be one right, all the way to the left
	
	while(curr!=NULL)
	{
		sp=successor;
		successor=curr;
		curr= curr->left;
	}
	
	if(successor!= d->right)//decendant of the right child
	{
		sp->left = successor->right;
		successor->right = d->right;
	}
	
	return successor;
}	

template<class T>	
bool BST<T>::contains(T d)
{
	TreeNode<T> *node= findByData(d, root);
	
	if(node==NULL)
		return false;
	return true;
}

template<class T>
TreeNode<T>* BST<T>::findByData(T d, TreeNode<T>*node)
{
	if(node==NULL)
		return node;
	if(d==node->data)
		return node;
	
	TreeNode<T>* temp=findByData(d, node->left);
	if(temp==NULL)
		temp=findByData(d, node->right);
	return temp;
}

template<class T>
bool BST<T>::hasKey(int k)
{
	TreeNode<T>* temp=findByKey(k,root);
	if(temp==NULL)
		return false;
	return true;
}

template<class T>
bool BST<T>::isEmpty()
{
	if(root==NULL)
		return true;
	return false;
}

template<class T>
TreeNode<T>* BST<T>::getMax()
{
	TreeNode<T> *curr=root;
	if(root==NULL){
		return NULL;
	}
	
	while(curr->right !=NULL)
	{
		curr=curr->right;
	}
	
	return curr;
}

template<class T>	
TreeNode<T>* BST<T>::getMin()
{
	TreeNode<T> *curr=root;
	if(root==NULL){
		return NULL;
	}
	
	while(curr->left !=NULL)
	{
		curr=curr->left;
	}
	
	return curr;
}	

template<class T>
TreeNode<T>* BST<T>::getLeft(TreeNode<T>* node)
{
	if(node==NULL)
		return NULL;
	return node->left;
}

template<class T>
TreeNode<T>* BST<T>::getRight(TreeNode<T>* node)
{
	if(node==NULL)
		return NULL;
	return node->right;
}

template<class T>
TreeNode<T>* BST<T>::getRoot()
{
	return root;
}

template<class T>
void BST<T>::printTree()
{
	printTree(root);
}

template<class T>
void BST<T>::printTree(TreeNode<T> *node)
{
	if(node==NULL)
		return;
	printTree(node->left);
	cout<<node->data<<endl;
	printTree(node->right);
}

/**
*End BST Implementation
**/
```
Now the same, but in the contorted, wordy and needlessly convoluted Haskell Language:

```haskell
    data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)  

    singleton :: a -> Tree a  
    singleton x = Node x EmptyTree EmptyTree  
      
    treeInsert :: (Ord a) => a -> Tree a -> Tree a  
    treeInsert x EmptyTree = singleton x  
    treeInsert x (Node a left right)   
        | x == a = Node x left right  
        | x < a  = Node a (treeInsert x left) right  
        | x > a  = Node a left (treeInsert x right)  
    
    mergeTrees :: (Ord a) => Tree a -> Tree a -> Tree a
    mergeTrees EmptyTree EmptyTree = EmptyTree
    mergeTrees EmptyTree (Node a left right) = Node a left right
    mergeTrees (Node a left right) EmptyTree = Node a left right
    mergeTrees (Node a left right) t = mergeTrees $ mergeTrees left $ mergeTrees right $ treeInsert a t


    treeElem :: (Ord a) => a -> Tree a -> Bool  
    treeElem x EmptyTree = False  
    treeElem x (Node a left right)  
        | x == a = True  
        | x < a  = treeElem x left  
        | x > a  = treeElem x right  
    
    treeDelete :: (Ord a) => a -> Tree a -> Tree a
    treeDelete x EmptyTree = EmptyTree
    treeDelete x (Node a left right)
        | x == a = mergeTrees left right
        | x < a = Node a (treeDelete x left) right
        | x > a = Node a left $treeDelte x right
```