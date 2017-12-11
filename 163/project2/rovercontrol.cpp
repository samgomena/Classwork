#include <iostream>
#include <fstream>
#include <cstring>
#include "rover.h"

using namespace std;

int main(int argc, char** argv) {

    // Set up the program constants
    const int NUMBER_OF_ROVERS = 7;
    const int MAX_COMMAND_LENGTH = 16;
    const int MAX_RESULTS = 128;
    const char* CMD_END = "END";
    const char* CMD_DEPLOY = "DEPLOY";
    const char* CMD_MOVE = "MOVE";
    const char* CMD_CORESCAN = "CORESCAN";
    const char* CMD_DOCK = "DOCK";

    rover** rovers;

    if (argc != 2) {
	cout << "Usage: " << argv[0] << " <datafile>" << endl;
	return(0);
    }

    // Create the rovers
    rovers = new rover*[NUMBER_OF_ROVERS];
    for (int i=0;i<NUMBER_OF_ROVERS;i++)
    {
	rovers[i] = new rover(i+1,MAX_RESULTS);
    }

    // Read the data
    char* datafile = argv[1];
    ifstream infile(datafile);
    char command[MAX_COMMAND_LENGTH];
    int roverID;
    int commandArg1;
    int commandArg2;

    infile >> command;
    while (strcmp(command,CMD_END) != 0) {
	infile >> roverID;

	if (strcmp(command,CMD_DEPLOY) == 0)
	{
	    rovers[roverID-1]->deploy();
	}
	if (strcmp(command,CMD_MOVE) == 0)
	{
	    infile >> commandArg1;
	    infile >> commandArg2;
	    rovers[roverID-1]->move(commandArg1, commandArg2);
	}
	if (strcmp(command,CMD_CORESCAN) == 0)
	{
	    rovers[roverID-1]->corescan();
	}
	if (strcmp(command,CMD_DOCK) == 0)
	{
	    rovers[roverID-1]->dock();
	}

	infile >> command;
    }

    // Free up memory for the allocated rovers
    for (int i=0;i<NUMBER_OF_ROVERS;i++)
    {
	if (rovers[i] != nullptr)
	{
	    delete rovers[i];
	    rovers[i] = nullptr;
	}
    }
    delete [] rovers;
}
