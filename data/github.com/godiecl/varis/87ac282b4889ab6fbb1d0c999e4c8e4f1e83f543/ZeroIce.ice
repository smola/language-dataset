/**
 * (c) 2019 FIC-R Mitigación del Riesgo Asociado a procesos volcánicos en la Region de Antofagasta
 */
#pragma once

/**
 * The ZeroIce Model
 */
["java:package:cl.ucn.ckelar.varis", "cs:namespace:CL.UCN.CKELAR.VARIS.Server"]
module ZeroIce {

    /**
     * The Model
     */
    module Model {

        // The type of Volcano
        // https://en.wikipedia.org/wiki/Stratovolcano
        enum Type {
            Caldera,
            CinderCone,
            Complex,
            Cryovolcano,
            FissureVent,
            LavaCone,
            LavaDome,
            Maar,
            Mud,
            ParasitiCone,
            PyroclasticCone,
            PyroclasticShield,
            RootlessCone,
            Shield,
            Somma,
            Stratovolcano,
            Subglacial,
            Submarine,
            Super,
            VolcanicVone
        };

        /**
         * The Volcano
         */
        ["cs:property"]
        class Volcano {

            // 
            string id;

            // GPV: 355100
            short gpv;

            // Lascar
            string name;

            // Stratovolcano
            string type;

            // -23.4525342
            float latitude;

            // -67.7958009
            float longitude;

            // 5592 mt
            short elevation;

            // 2
            int region;

            // Region de Antofagasta
            string localizacion;

            // ?!?
            string emissivity;

            // ?!?
            string transmissivity;

        };

        /**
         * The Persona
         */
        ["cs:property"]
        class Person {

            string id;

            string fistname;

            string lastname;

            string email;

            // Cargo
            string position;

            string institution;

            string country;
        }

        /**
         * List of Volcano
         */
        ["java:type:java.util.ArrayList<cl.ucn.ckelar.varis.ZeroIce.Model.Volcano>:java.util.List<cl.ucn.ckelar.varis.ZeroIce.Model.Volcano>", "cs:generic:List"]
        sequence<Volcano> Volcanos;

        /**
         * List of Persons
         */
        ["java:type:java.util.ArrayList<cl.ucn.ckelar.varis.ZeroIce.Model.Person>:java.util.List<cl.ucn.ckelar.varis.ZeroIce.Model.Person>", "cs:generic:List"]
        sequence<Person> People;

    };

    /**
     * Controllers
     */
    module Controller {

        /**
         * Operaciones del Sistema
         */
        interface IServerModel {

            /**
             * List of Volcano
             */
             idempotent Model::Volcanos getVolcanos();

            /**
             * List of Person.
             */
            idempotent Model::People getPeople();
    
        };

    };

};