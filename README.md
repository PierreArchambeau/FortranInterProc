# FortranInterProc

Initiation of a Fortran module to enable Interprocess Communication through pipes under Windows.
Based on F2003 standard

Example :

Server console :

    program Consoleserver

    use pipes
    implicit none
    
    type(pipe_windows) myserver
    character(len=:), allocatable :: myout
   
    myserver%filename = '\\.\pipe\pipetest'
    call myserver%create_pipe
    call myserver%read_data(myout,15)
    call myserver%close_pipe
    
    end program 
    
Client :

    program Consoleclient
    
    use pipes
    implicit none
    
    type(pipe_windows) myclient
    character(len=250) myout

    myclient%filename = '\\.\pipe\pipetest'
    call myclient%connect_pipe
    myout='test'
    call myclient%write_data(myout)
    call myclient%close_pipe

    end program

It is also possible to redirect console output  
    >&\\.\pipe\pipetest
    

Enjoy!
