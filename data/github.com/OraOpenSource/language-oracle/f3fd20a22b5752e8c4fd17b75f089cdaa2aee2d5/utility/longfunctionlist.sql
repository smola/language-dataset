set serveroutput on

declare

    type t_proc is record (
        object_name all_procedures.object_name%type,
        procedure_name all_procedures.procedure_name%type
    );
    type c_proc is table of t_proc
        index by PLS_INTEGER;

    l_system_procs c_proc;
    l_current_name all_procedures.object_name%type;
    l_total_len NUMBER;
    procedure print_info(
        p_procedure_name in varchar2,
        p_length in NUMBER
    )
    as
    begin

        if p_length > 4000
        then
            dbms_output.put_line(p_procedure_name || ',' || p_length);
        end if;

    end print_info;
begin
    l_current_name := '';

    select object_name, procedure_name
    bulk collect into l_system_procs
    from all_procedures
    where procedure_name is not null
    order by object_name;

    for i in 1..l_system_procs.COUNT
    LOOP
        --different name, so reset the count and log the count
        if l_system_procs(i).object_name != l_current_name or l_current_name is NULL
        then

            print_info(l_current_name , l_total_len);
            l_total_len := 0;
            l_current_name := l_system_procs(i).object_name;
        end if;


        l_total_len := l_total_len + length(l_system_procs(i).procedure_name) + 1;

    END LOOP;
    print_info(l_current_name , l_total_len);

end;
