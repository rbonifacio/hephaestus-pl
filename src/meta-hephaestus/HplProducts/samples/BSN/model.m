Root : Monitoring [Storage] :: _Root ;

Monitoring : Sensor+ SensorInformation+ :: _Monitoring ;

Sensor : SPO2
	| ECG
	| TEMP
	| ACC ;

SensorInformation : Oxygenation
	| PulseRate
	| Temperature
	| Position
	| Fail ;

Storage : SQLite
	| Memory ;

%%

Oxygenation implies SPO2 ;
Position implies ACC ;
Temperature implies TEMP ;
Fail implies ACC ;
PulseRate implies SPO2 or ECG ;

