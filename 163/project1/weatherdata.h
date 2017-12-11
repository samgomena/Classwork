#ifndef WEATHERDATA_H
#define WEATHERDATA_H

class WeatherData {
public:
    WeatherData();
    WeatherData(int, double, double);
    int getTime() const;
    double getTemp() const;
    double getWind() const;

private:
    int time;
    double temp;
    double wind;

};
#endif
