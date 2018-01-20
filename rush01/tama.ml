class tama =
    object (self)
        val health : int = 100
        val energy : int = 100
        val hygiene : int = 100
        val happyness : int = 100
        val file_name : string = "save.itama"

        method health = health
        method energy = energy
        method hygiene = hygiene
        method happyness = happyness

        method effect delta_health delta_energy delta_hygiene delta_happyness =
            let get_stat value =
                max 0 (min 100 value) in
            match self#is_live () with
            | false       -> {<>}
            | true        -> ({< health    = get_stat (health + delta_health);
                                 energy    = get_stat (energy + delta_energy);
                                 hygiene   = get_stat (hygiene + delta_hygiene);
                                 happyness = get_stat (happyness + delta_happyness) >})

        method eat () =
            self#effect 25 ~-10 ~-20 5
        method thunder () =
            self#effect ~-20 25 0 ~-20
        method bath () =
            self#effect ~-20 ~-10 25 5
        method kill () =
            self#effect ~-20 ~-10 0 20

        method second_passe () =
            self#effect ~-1 0 0 0

        method is_live () =
            if health <= 0 || energy <= 0 || hygiene <= 0 || happyness <= 0
            then false
            else true

        method load () =
            let get_value file =
                let value = int_of_string (input_line file) in
                if value > 100 || value < 0
                then failwith "Invalide file"
                else value in
            let opened_file = open_in file_name in
            let save_health = get_value opened_file
            and save_energy = get_value opened_file
            and save_hygiene = get_value opened_file
            and save_happyness = get_value opened_file in
            close_in opened_file;
            ({< health    = save_health;
                energy    = save_energy;
                hygiene   = save_hygiene;
                happyness = save_happyness >})
        method save () =
            let write_file file value =
                output_string file ((string_of_int value) ^ "\n") in
            let opened_file = open_out file_name in
            write_file opened_file health;
            write_file opened_file energy;
            write_file opened_file hygiene;
            write_file opened_file happyness;
            close_out opened_file

        method to_string () =
            ("health    = " ^ (string_of_int health) ^ "\n" ^
             "energy    = " ^ (string_of_int energy) ^ "\n" ^
             "hygiene   = " ^ (string_of_int hygiene) ^ "\n" ^
             "happyness = " ^ (string_of_int happyness))
    end
