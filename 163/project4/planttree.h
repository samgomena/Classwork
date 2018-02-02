#ifndef _PLANTTREE
#define _PLANTTREE

#include "treenode.h"

class planttree {
private:
    TreeNode* _root;
    plant* maxPlant;

    void destroyTree(TreeNode *);

public:
    planttree();
//    planttree(const planttree&);
    ~planttree();

    const planttree& operator= (const planttree&);
    void copyTree(TreeNode *, TreeNode*);

    void display();
    void setRoot(plant);
    void addChildren(const plant &, const plant, const plant);

    const plant * findBestGrowth();
    const plant * findBestNutrition();
    const plant * findBestWater();


    void preOrderPrint(TreeNode *, int) const;
    TreeNode *inOrderInsert(TreeNode *&, TreeNode *, const char *) const;
    void postOrderPlantLookup(TreeNode *&, TreeNode *, int&, int) const;
};

void spaceIt(int);
#endif