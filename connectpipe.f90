
!> Interprocess communication On Windows
!! On Linux, use mkfifo
!! @date 2017-01-26
!! @author Pierre Archambeau
module pipes

    USE IFWIN
    use iso_c_binding, only: c_ptr, c_loc, c_f_pointer

    !> Server initiating the pipe
    type pipe_windows

        integer(INT_PTR_KIND())         :: handle           = 0     !< handle of the pipe
        character(len=:), allocatable   :: filename                 !< name of the pipe ex.: \\.\pipe\mypipe
        integer(kind=4)                 :: size_buffer_in   = 256   !< size of the inbuffer
        integer(kind=4)                 :: size_buffer_out  = 256   !< size of the outbuffer

        contains
            !> creation of a pipe
            procedure create_pipe
            !> connection to a pipe
            procedure connect_pipe
            !> read data
            procedure read_data
            !> write data
            procedure write_data
            !> close pipe --> close handle
            procedure close_pipe
    end type

contains

    !> Creation of a pipe
    !! @date 2017-01-26
    !! @author Pierre Archambeau
    subroutine create_pipe(this)
        class(pipe_windows) :: this
        type(T_SECURITY_ATTRIBUTES) :: NULL_SEC_ATTR

        this%handle = CreateNamedPipe(this%filename//char(0),             &
                          PIPE_ACCESS_DUPLEX,       &
                          PIPE_TYPE_MESSAGE+PIPE_READMODE_MESSAGE+PIPE_WAIT,           &
                          PIPE_UNLIMITED_INSTANCES,        &
                          this%size_buffer_out,          &
                          this%size_buffer_in,           &
                          NMPWAIT_USE_DEFAULT_WAIT,       &
                          null)

        return
    end subroutine

    !> Connection to a pipe
    !! @date 2017-01-26
    !! @author Pierre Archambeau
    subroutine connect_pipe(this)
        class(pipe_windows) :: this
        type(T_SECURITY_ATTRIBUTES) :: NULL_SEC_ATTR
        integer(kind=4) :: ierror

        ierror  = WaitNamedPipe(this%filename//char(0),NMPWAIT_USE_DEFAULT_WAIT)

        if(ierror==0) return
        
        this%handle = CreateFile(this%filename//char(0),             &
                          GENERIC_READ+GENERIC_WRITE,       &
                          FILE_SHARE_READ+FILE_SHARE_WRITE,           &
                          NULL_SEC_ATTR,        &
                          OPEN_EXISTING,          &
                          FILE_ATTRIBUTE_NORMAL,           &
                          0 )
    end subroutine

    !> Close the pipe
    !! @date 2017-01-26
    !! @author Pierre Archambeau
    subroutine close_pipe(this)
        class(pipe_windows) :: this
        integer(kind=4) :: ierror

        ierror          = CloseHandle(this%handle)
        this%handle     = 0
        this%filename   = ''

    end subroutine

    !> Read the data into the pipe
    !! @param out_str : string to receive
    !! @param attsize : length of the expected string
    !! @date 2017-01
    !! @author Pierre Archambeau
    subroutine read_data(this,out_str,attsize)
        class(pipe_windows) this
        logical :: to_read = .true.
        integer(kind=4) :: nb_read
        integer(kind=4) :: istatus
        integer(kind=4) :: attsize, cursize
        character(len=this%size_buffer_in) :: buffer
        character(len=:), allocatable :: out_str

        if(this%handle==0) return

        cursize = 0
        out_str = ''
        do while(to_read)
            if(ConnectNamedPipe(this%handle,null)/=0) then

                do while(cursize<attsize)
                    istatus=ReadFile(this%handle,loc(buffer),this%size_buffer_in,loc(nb_read),null)
                    cursize = cursize+len_trim(buffer)

                    nb_read = 0
                    out_str = out_str // trim(buffer)
                enddo

                to_read = .false.

            endif
            call Sleep(50)
        enddo

    end subroutine

    !> Write the data into the pipe
    !! @param in_str : string to pass
    !! @date 2017-01
    !! @author Pierre Archambeau
    subroutine write_data(this,in_str)
        class(pipe_windows) this
        integer(kind=4) :: nb_write
        integer(kind=4) :: istatus
        character(len=this%size_buffer_in) :: buffer
        character(len=*) :: in_str

        if(this%handle==0) return
        
        buffer = in_str

        istatus=WriteFile(this%handle,loc(buffer),this%size_buffer_out,loc(nb_write),null)

    end subroutine

end module
