#include "treenode.h"


TreeNode::TreeNode(const plant & rootHopefully) : _left(nullptr), _right(nullptr), plantData(rootHopefully){}

TreeNode::TreeNode(const plant & newPlant, TreeNode *left, TreeNode *right)
        : _left(left), _right(right), plantData(newPlant) {}

TreeNode::~TreeNode() {}

void TreeNode::setItem(const plant &newPlant) {
    plantData = newPlant;
}

plant TreeNode::getItem() const {
    return plantData;
}

TreeNode *TreeNode::getLeftChild() const {
    return _left;
}

TreeNode *TreeNode::getRightChild() const {
    return _right;
}

void TreeNode::setLeftChild(TreeNode *left) {
    _left = left;
}

void TreeNode::setRightChild(TreeNode *right) {
    _right = right;
}

bool TreeNode::isLeaf() const {
    return getRightChild() == nullptr && getLeftChild() == nullptr;
}
