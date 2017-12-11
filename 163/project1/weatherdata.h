#ifndef WEATHERDATA_H
#define WEATHERDATA_H

class WeatherData {
public:
    WeatherData();
    WeatherData(int, double, double);
    // WeatherData(const WeatherData& copyData);
//    ~WeatherData();

//    void setTime(int);
//    void setTemp(int);
//    void setWind(int);

    int getTime() const;
    double getTemp() const;
    double getWind() const;
	
//	WeatherData& operator = (const WeatherData &rhs);

private:
    int time;
    double temp;
    double wind;

};
#endif
