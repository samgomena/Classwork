#include <algorithm>
#include <iostream>
#include <cstring>

#include "planttree.h"

using namespace std;

planttree::planttree() : _root(nullptr), maxPlant(nullptr) {}

//planttree::planttree(const planttree& pt):_root(nullptr) {
//    if(pt.maxPlant) {
//        maxPlant = pt.maxPlant;
//    }
//    *this = pt;
//}

planttree::~planttree() {
//    destroyTree(_root);
    if(maxPlant) {
        delete maxPlant;
    }
}

const planttree& planttree::operator= (const planttree& pt) {
    if(this == &pt) {
        return *this;
    } else {
        destroyTree(_root);
        copyTree(_root, pt._root);
        return *this;
    }
}

void planttree::copyTree(TreeNode *dest, TreeNode* _root) {
    if(_root) {
        dest = new TreeNode(_root->getItem());
        copyTree(dest->getLeftChild(), _root->getLeftChild());
        copyTree(dest->getRightChild(), _root->getRightChild());
    }
    else {
        dest = nullptr;
    }
}

void planttree::destroyTree(TreeNode * treenode) {
    if (treenode->getLeftChild() != nullptr && treenode->getRightChild() != nullptr) {
            destroyTree(treenode->getLeftChild());
            destroyTree(treenode->getRightChild());
            delete treenode;
    }
}

void planttree::setRoot(plant root) {
    TreeNode *newLeaf = new TreeNode(root);
    _root = newLeaf;
}

void planttree::addChildren(const plant &parent, const plant left, const plant right) {
    TreeNode *legalGuardian = nullptr;
    inOrderInsert(legalGuardian, _root, parent.getId());
    TreeNode *lefty = new TreeNode(left);
    TreeNode *righty = new TreeNode(right);

    legalGuardian->setLeftChild(lefty);
    legalGuardian->setRightChild(righty);
}

void planttree::display() {
    preOrderPrint(_root, 0);
}

const plant *planttree::findBestGrowth() {
    TreeNode* plantWrapper = nullptr;

    int max = 0;
    postOrderPlantLookup(plantWrapper, _root, max, 0);
    if(maxPlant) {
        delete maxPlant;
    }
    maxPlant = new plant(plantWrapper->getItem());
    return maxPlant;
}

const plant *planttree::findBestNutrition() {
    TreeNode* plantWrapper = nullptr;

    int max = 0;
    postOrderPlantLookup(plantWrapper, _root, max, 1);
    if(maxPlant) {
        delete maxPlant;
    }
    maxPlant = new plant(plantWrapper->getItem());
    return maxPlant;
}

const plant *planttree::findBestWater() {
    TreeNode* plantWrapper = nullptr;

    int max = 0;
    postOrderPlantLookup(plantWrapper, _root, max, 2);
    if(maxPlant) {
        delete maxPlant;
    }
    maxPlant = new plant(plantWrapper->getItem());
    return maxPlant;
}

/*  0 = Growth
    1 = Nutrition
    2 = Water
 */
void planttree::postOrderPlantLookup(TreeNode *&plant, TreeNode *leaf, int &max, int whichOne) const {
    if(whichOne == 0) {
        if(leaf->getItem().getGrowth() > max){
            max = leaf->getItem().getGrowth();
            plant = leaf;
        }
    } else if (whichOne == 1) {
        if(leaf->getItem().getNutrition() > max){
            max = leaf->getItem().getNutrition();
            plant = leaf;
        }
    } else if (whichOne == 2) {
        if(leaf->getItem().getWater() > max){
            max = leaf->getItem().getWater();
            plant = leaf;
        }
    }

    if(leaf->getLeftChild() != nullptr && leaf->getRightChild() != nullptr) {
        postOrderPlantLookup(plant, leaf->getLeftChild(), max, whichOne);
        postOrderPlantLookup(plant, leaf->getRightChild(), max, whichOne);
    }
}

TreeNode *planttree::inOrderInsert(TreeNode *&plant, TreeNode *leaf, const char *plantName) const {
    if(strcmp(plantName, leaf->getItem().getId()) == 0) {
        plant = leaf;
    }
    if (leaf->getLeftChild() != nullptr && leaf->getRightChild() != nullptr) {
        inOrderInsert(plant, leaf->getRightChild(), plantName);
        inOrderInsert(plant, leaf->getLeftChild(), plantName);
    }
}

void planttree::preOrderPrint(TreeNode *node, int depth) const {
    if (node != nullptr) {
        spaceIt(depth);
        cout << node->getItem() << endl;
        preOrderPrint(node->getLeftChild(), depth + 1);
        preOrderPrint(node->getRightChild(), depth + 1);
    }
}

// Helper function for pretty printing.
void spaceIt(int depth) {
    int i;
    for(i = 0; i < depth; i++) {
        cout << "  ";
    }
}