/**
* Jorge Silva
*/
module SmartHome {
    interface Environment {
        //Get Data
        double getTemperature(int idDorm);
        double getHumidity(int idDorm);
        double getCO2(int idDorm);
        double getGas(int idDorm);
        double getLuminity(int idDorm);
        //Turn On
        void turnOnFans(int idDorm);
        void turnOnExtracts(int idDorm);
        void turnOnBulb(int idDorm, int idBulb);
        void turnOnLamp(int idDorm, int idLamp);
        //Turn Off
        void turnOffFans(int idDorm);
        void turnOffExtracts(int idDorm);
        void turnOffBulb(int idDorm, int idBulb);
        void turnOffLamp(int idDorm, int idLamp);
    };

    interface Security {
        //Monitoring
        void monitorCO2(int idDorm);
        void monitorGas(int idDorm);
        void detectSmoke(int idDorm);
        void detectSmoke();
        void detectMotion(int idDorm);
        void detectMotion();
        void detectInteractionWinDoo(int idDorm);
        void detectInteractionWinDoo();
        void detectFlame(int idDorm);
        void detectFlame();
        //Turn On
        void turnOnAlarm(int idAlarm);
        void turnOnAllAlarms();
        //Turn Off
        void turnOffAlarm(int idAlarm);
        void turnOffAllAlarms();
    };

    interface Utils {
        void getRoomsList();
        void getRoom(int idDorm);
        void getSumarry();
    };
};