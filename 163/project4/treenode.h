#ifndef _TREENODE
#define _TREENODE

#include "plant.h"

class TreeNode {
private:
    TreeNode *_left;
    TreeNode *_right;
    plant plantData;

public:
    ~TreeNode();
    TreeNode(const plant&);
    TreeNode(const plant&, TreeNode*, TreeNode*);
    
    void setItem(const plant& newPlant);
    plant getItem() const;
    bool isLeaf() const;
    TreeNode* getLeftChild() const;
    TreeNode* getRightChild() const;
    void setLeftChild(TreeNode* left);
    void setRightChild(TreeNode* right);

    void setPlant(const plant &plant);
};
#endif