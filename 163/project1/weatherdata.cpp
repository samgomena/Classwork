#include "weatherdata.h"

WeatherData::WeatherData() : time(0), temp(0), wind(0){};

WeatherData::WeatherData(int _time, double _temp, double _wind) : time(_time), temp(_temp), wind(_wind){};

//WeatherData::~WeatherData(){};

//    void WeatherData::setTime(int _time) {
//        time = _time;
//    }
//
//    void WeatherData::setTemp(int _temp) {
//        temp = _temp;
//    }
//    void WeatherData::setWind(int _wind) {
//        wind = _wind;
//    };

    int WeatherData::getTime() const {
        return time;
    }
    
    double WeatherData::getTemp() const {
        return temp;
    }
    
    double WeatherData::getWind() const {
        return wind;
    }

//WeatherData& WeatherData::operator=(const WeatherData &rhs) {
//	if(this == &rhs) {
//        return *this;
//    }
//    this->time = rhs.time;
//    temp = rhs.temp;
//    wind = rhs.wind;
//	return *this;
//}
