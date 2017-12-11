#ifndef SCANDATA_H
#define SCANDATA_H

#define NUM_SCANDATA_VALUES 1000

class scandata {
private:
    static const int m_data[];

public:
    static int getScandata(int x, int y);
};


#endif
