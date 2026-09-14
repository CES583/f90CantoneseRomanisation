!=======================================================================
! yutyut.f90 - Cantonese Yựtyựt and Cantonese Zhuyin converter
!
! Reads list.tsv (CH, UCODE, JP, INIT, FINL, TONE, DESC, DESC_JP)
! Converts each Chinese character to:
!   - Yựtyựt (越式粵拼) romanisation
!   - Cantonese Zhuyin (廣東話注音符號)
!
! Compile:  gfortran -O2 -o yutyut yutyut.f90
! Run:      ./yutyut              (interactive)
!           ./yutyut input.txt    (batch)
!           echo "中國人" | ./yutyut
!=======================================================================
program yutyut_converter
    implicit none

    type :: dict_entry
        character(len=8)   :: ch
        character(len=16)  :: jp
        character(len=8)   :: init
        character(len=8)   :: finl
        integer            :: tone
    end type

    type(dict_entry), allocatable :: dict(:)
    integer :: n_dict, ios, i, n_args
    character(len=4096) :: input_line
    character(len=512)  :: arg

    !-------------------------------------------------------------------
    ! Load dictionary
    !-------------------------------------------------------------------
    call load_dict('list.tsv', dict, n_dict)
    if (n_dict == 0) then
        write(*,'(A)') 'Error: could not load list.tsv'
        stop 1
    end if
    write(*,'(A,I0,A)') 'Loaded ', n_dict, ' entries from list.tsv'

    !-------------------------------------------------------------------
    ! Decide mode: interactive or batch
    !-------------------------------------------------------------------
    n_args = command_argument_count()
    if (n_args > 0) then
        call get_command_argument(1, arg)
        call process_file(trim(arg), dict, n_dict)
    else
        write(*,'(A)') 'Enter Chinese text (empty line to quit):'
        do
            write(*,'(A)', advance='no') '> '
            read(*, '(A)', iostat=ios) input_line
            if (ios /= 0) exit
            if (len_trim(input_line) == 0) exit
            call process_line(input_line, dict, n_dict)
            write(*,*)
        end do
    end if

    deallocate(dict)

contains

!=======================================================================
! Load dictionary from TSV file
!=======================================================================
subroutine load_dict(fname, d, n)
    character(len=*), intent(in) :: fname
    type(dict_entry), allocatable, intent(out) :: d(:)
    integer, intent(out) :: n
    integer :: u, i, ios
    character(len=4096) :: ln

    open(newunit=u, file=fname, status='old', action='read', iostat=ios)
    if (ios /= 0) then
        n = 0
        return
    end if

    ! First pass: count non-empty lines (excluding header)
    n = 0
    do
        read(u, '(A)', iostat=ios) ln
        if (ios /= 0) exit
        if (len_trim(ln) == 0) cycle
        if (ln(1:2) == 'CH' .and. index(ln, achar(9)) > 0) cycle  ! header
        n = n + 1
    end do

    allocate(d(n))
    rewind(u)

    i = 0
    do
        read(u, '(A)', iostat=ios) ln
        if (ios /= 0) exit
        if (len_trim(ln) == 0) cycle
        if (ln(1:2) == 'CH' .and. index(ln, achar(9)) > 0) cycle
        i = i + 1
        call parse_tsv_line(ln, d(i))
    end do
    close(u)
    n = i
end subroutine load_dict

!=======================================================================
! Parse one TSV line into a dict_entry
!=======================================================================
subroutine parse_tsv_line(ln, e)
    character(len=*), intent(in) :: ln
    type(dict_entry), intent(out) :: e
    character(len=1024) :: fields(8)
    integer :: pos, tab_pos, fn, ios

    do fn = 1, 8
        fields(fn) = ''
    end do
    e%ch   = ''
    e%jp   = ''
    e%init = ''
    e%finl = ''
    e%tone = 0

    pos = 1
    fn  = 1
    do while (fn <= 8 .and. pos <= len(ln))
        tab_pos = index(ln(pos:), achar(9))
        if (tab_pos == 0) then
            fields(fn) = ln(pos:)
            exit
        else
            fields(fn) = ln(pos:pos+tab_pos-2)
            pos = pos + tab_pos
            fn  = fn + 1
        end if
    end do

    e%ch   = trim(fields(1))
    e%jp   = trim(fields(3))
    e%init = trim(fields(4))
    e%finl = trim(fields(5))
    if (len_trim(fields(6)) > 0) then
        read(fields(6), *, iostat=ios) e%tone
    end if
end subroutine parse_tsv_line

!=======================================================================
! Process one line of Chinese text
!=======================================================================
subroutine process_line(ln, d, n)
    character(len=*), intent(in) :: ln
    type(dict_entry), intent(in) :: d(:)
    integer, intent(in) :: n
    integer :: pos, char_len, j, len_in
    character(len=8) :: ch_buf
    character(len=:), allocatable :: y, z
    logical :: found

    len_in = len_trim(ln)
    pos = 1
    do while (pos <= len_in)
        call utf8_len(ichar(ln(pos:pos)), char_len)
        if (char_len == 0 .or. pos + char_len - 1 > len_in) then
            pos = pos + 1
            cycle
        end if

        ch_buf = ''
        ch_buf(1:char_len) = ln(pos:pos+char_len-1)
        pos = pos + char_len

        found = .false.
        do j = 1, n
            if (trim(d(j)%ch) == trim(ch_buf)) then
                call to_yutyut(d(j)%init, d(j)%finl, d(j)%tone, y)
                call to_zhuyin(d(j)%init, d(j)%finl, d(j)%tone, z)
                write(*,'(A,A,A,A,A,A,A)') '  ', trim(d(j)%ch), &
                    ' [', trim(d(j)%jp), ']  ', trim(y), '  |  ', trim(z)
                found = .true.
            end if
        end do
        if (.not. found) then
            write(*,'(A,A)') '  ?  ', trim(ch_buf)
        end if
    end do
end subroutine process_line

!=======================================================================
! Batch mode: read a file
!=======================================================================
subroutine process_file(fname, d, n)
    character(len=*), intent(in) :: fname
    type(dict_entry), intent(in) :: d(:)
    integer, intent(in) :: n
    integer :: u, ios
    character(len=4096) :: ln

    open(newunit=u, file=fname, status='old', action='read', iostat=ios)
    if (ios /= 0) then
        write(*,'(A,A)') 'Error: cannot open ', trim(fname)
        return
    end if
    do
        read(u, '(A)', iostat=ios) ln
        if (ios /= 0) exit
        if (len_trim(ln) > 0) call process_line(ln, d, n)
    end do
    close(u)
end subroutine process_file

!=======================================================================
! UTF-8 first-byte length
!=======================================================================
subroutine utf8_len(b, n)
    integer, intent(in) :: b
    integer, intent(out) :: n
    if (b < 128) then
        n = 1
    else if (b >= 192 .and. b < 224) then
        n = 2
    else if (b >= 224 .and. b < 240) then
        n = 3
    else if (b >= 240 .and. b < 248) then
        n = 4
    else
        n = 0
    end if
end subroutine utf8_len

!=======================================================================
! Yựtyựt conversion
!=======================================================================
subroutine to_yutyut(init, finl, tone, out)
    character(len=*), intent(in) :: init, finl
    integer, intent(in) :: tone
    character(len=:), allocatable, intent(out) :: out
    character(len=:), allocatable :: i_out, f_out
    logical :: pal

    !--- palatalisation: z/c/s + i- or yu- -> zh/ch/sh
    pal = .false.
    if (trim(finl) == 'yu' .or. trim(finl) == 'yun' .or. trim(finl) == 'yut') then
        pal = .true.
    else if (len_trim(finl) >= 1) then
        if (finl(1:1) == 'i') pal = .true.
    end if

    !--- initial
    select case(trim(init))
    case('');   i_out = ''
    case('b');  i_out = 'b'
    case('p');  i_out = 'p'
    case('m');  i_out = 'm'
    case('f');  i_out = 'f'
    case('d');  i_out = 'd'
    case('t');  i_out = 't'
    case('n');  i_out = 'n'
    case('l');  i_out = 'l'
    case('g');  i_out = 'g'
    case('k');  i_out = 'k'
    case('ng'); i_out = 'ng'
    case('h');  i_out = 'h'
    case('j');  i_out = 'y'
    case('w');  i_out = 'w'
    case('gw'); i_out = 'gu'
    case('kw'); i_out = 'ku'
    case('z');  if (pal) then; i_out = 'zh'; else; i_out = 'z'; end if
    case('c');  if (pal) then; i_out = 'ch'; else; i_out = 'c'; end if
    case('s');  if (pal) then; i_out = 'sh'; else; i_out = 's'; end if
    case default; i_out = trim(init)
    end select

    call yutyut_final(finl, tone, f_out)
    out = i_out // f_out
end subroutine to_yutyut

!=======================================================================
! Yựtyựt final + tone
!=======================================================================
subroutine yutyut_final(finl, tone, out)
    character(len=*), intent(in) :: finl
    integer, intent(in) :: tone
    character(len=:), allocatable, intent(out) :: out
    character(len=:), allocatable :: v

    select case(trim(finl))
    ! aa-group: tone on plain 'a'
    case('aa');   call vton('a', tone, v);  out = v
    case('aai');  call vton('a', tone, v);  out = v // 'i'
    case('aau');  call vton('a', tone, v);  out = v // 'u'
    case('aam');  call vton('a', tone, v);  out = v // 'm'
    case('aan');  call vton('a', tone, v);  out = v // 'n'
    case('aang'); call vton('a', tone, v);  out = v // 'ng'
    case('aap');  call vton('a', tone, v);  out = v // 'p'
    case('aat');  call vton('a', tone, v);  out = v // 't'
    case('aak');  call vton('a', tone, v);  out = v // 'k'

    ! a-group: tone on 'ǎ'
    case('ai');   call vton('ǎ', tone, v); out = v // 'i'
    case('au');   call vton('ǎ', tone, v); out = v // 'u'
    case('am');   call vton('ǎ', tone, v); out = v // 'm'
    case('an');   call vton('ǎ', tone, v); out = v // 'n'
    case('ang');  call vton('ǎ', tone, v); out = v // 'ng'
    case('ap');   call vton('ǎ', tone, v); out = v // 'p'
    case('at');   call vton('ǎ', tone, v); out = v // 't'
    case('ak');   call vton('ǎ', tone, v); out = v // 'k'

    ! e-group
    case('e');    call vton('e', tone, v);  out = v
    case('ei');   call vton('e', tone, v);  out = v // 'i'
    case('eng');  call vton('e', tone, v);  out = v // 'ng'
    case('ek');   call vton('e', tone, v);  out = v // 'k'
    case('ep');   call vton('e', tone, v);  out = v // 'p'
    case('et');   call vton('e', tone, v);  out = v // 't'
    case('eu');   call vton('e', tone, v);  out = v // 'u'

    ! i-group
    case('i');    call vton('i', tone, v);  out = v
    case('iu');   call vton('i', tone, v);  out = v // 'u'
    case('im');   call vton('i', tone, v);  out = v // 'm'
    case('in');   call vton('i', tone, v);  out = v // 'n'
    case('ing');  call vton('i', tone, v);  out = v // 'ng'
    case('ip');   call vton('i', tone, v);  out = v // 'p'
    case('it');   call vton('i', tone, v);  out = v // 't'
    case('ik');   call vton('i', tone, v);  out = v // 'k'

    ! o-group
    case('o');    call vton('o', tone, v);  out = v
    case('oi');   call vton('o', tone, v);  out = v // 'i'
    case('ou');   call vton('o', tone, v);  out = v // 'u'
    case('on');   call vton('o', tone, v);  out = v // 'n'
    case('ong');  call vton('o', tone, v);  out = v // 'ng'
    case('ot');   call vton('o', tone, v);  out = v // 't'
    case('ok');   call vton('o', tone, v);  out = v // 'k'

    ! u-group
    case('u');    call vton('u', tone, v);  out = v
    case('ui');   call vton('u', tone, v);  out = v // 'i'
    case('un');   call vton('u', tone, v);  out = v // 'n'
    case('ung');  call vton('u', tone, v);  out = v // 'ng'
    case('ut');   call vton('u', tone, v);  out = v // 't'
    case('uk');   call vton('u', tone, v);  out = v // 'k'

    ! ơ-group
    case('oe');   call vton('ơ', tone, v);  out = v
    case('oeng'); call vton('ơ', tone, v);  out = v // 'ng'
    case('oek');  call vton('ơ', tone, v);  out = v // 'k'
    case('eoi');  call vton('ơ', tone, v);  out = v // 'ư'
    case('eon');  call vton('ơ', tone, v);  out = v // 'n'
    case('eot');  call vton('ơ', tone, v);  out = v // 't'

    ! ư-group
    case('yu');   call vton('ư', tone, v);  out = v
    case('yun');  call vton('ư', tone, v);  out = v // 'n'
    case('yut');  call vton('ư', tone, v);  out = v // 't'

    ! syllabic nasals
    case('m')
        select case(tone)
        case(4); out = 'm'
        case(2); out = 'm'
        case(6); out = 'm'
        case default; out = 'm'
        end select
    case('ng')
        select case(tone)
        case(4); out = 'ng'
        case(5); out = 'ñg'
        case(6); out = 'ṇg'
        case default; out = 'ng'
        end select

    case default
        out = trim(finl)
    end select
end subroutine yutyut_final

!=======================================================================
! Apply tone to a base vowel
!=======================================================================
subroutine vton(base, tone, out)
    character(len=*), intent(in) :: base
    integer, intent(in) :: tone
    character(len=:), allocatable, intent(out) :: out

    select case(trim(base))
    case('a')
        select case(tone)
        case(1); out = 'a'
        case(2); out = 'ả'
        case(3); out = 'á'
        case(4); out = 'à'
        case(5); out = 'ã'
        case(6); out = 'ạ'
        case default; out = 'a'
        end select
    case('ǎ')
        select case(tone)
        case(1); out = 'ǎ'
        case(2); out = 'ẳ'
        case(3); out = 'ắ'
        case(4); out = 'ằ'
        case(5); out = 'ẵ'
        case(6); out = 'ặ'
        case default; out = 'ǎ'
        end select
    case('e')
        select case(tone)
        case(1); out = 'e'
        case(2); out = 'ẻ'
        case(3); out = 'é'
        case(4); out = 'è'
        case(5); out = 'ẽ'
        case(6); out = 'ẹ'
        case default; out = 'e'
        end select
    case('i')
        select case(tone)
        case(1); out = 'i'
        case(2); out = 'ỉ'
        case(3); out = 'í'
        case(4); out = 'ì'
        case(5); out = 'ĩ'
        case(6); out = 'ị'
        case default; out = 'i'
        end select
    case('o')
        select case(tone)
        case(1); out = 'o'
        case(2); out = 'ỏ'
        case(3); out = 'ó'
        case(4); out = 'ò'
        case(5); out = 'õ'
        case(6); out = 'ọ'
        case default; out = 'o'
        end select
    case('u')
        select case(tone)
        case(1); out = 'u'
        case(2); out = 'ủ'
        case(3); out = 'ú'
        case(4); out = 'ù'
        case(5); out = 'ũ'
        case(6); out = 'ụ'
        case default; out = 'u'
        end select
    case('ơ')
        select case(tone)
        case(1); out = 'ơ'
        case(2); out = 'ở'
        case(3); out = 'ớ'
        case(4); out = 'ờ'
        case(5); out = 'ỡ'
        case(6); out = 'ợ'
        case default; out = 'ơ'
        end select
    case('ư')
        select case(tone)
        case(1); out = 'ư'
        case(2); out = 'ử'
        case(3); out = 'ứ'
        case(4); out = 'ừ'
        case(5); out = 'ữ'
        case(6); out = 'ự'
        case default; out = 'ư'
        end select
    case default
        out = trim(base)
    end select
end subroutine vton

!=======================================================================
! Cantonese Zhuyin conversion
!=======================================================================
subroutine to_zhuyin(init, finl, tone, out)
    character(len=*), intent(in) :: init, finl
    integer, intent(in) :: tone
    character(len=:), allocatable, intent(out) :: out
    character(len=:), allocatable :: i_z, f_z, t_z
    logical :: pal

    pal = .false.
    if (trim(finl) == 'yu' .or. trim(finl) == 'yun' .or. trim(finl) == 'yut') then
        pal = .true.
    else if (len_trim(finl) >= 1) then
        if (finl(1:1) == 'i') pal = .true.
    end if

    !--- initial
    select case(trim(init))
    case('');   i_z = ''
    case('b');  i_z = 'ㄅ'
    case('p');  i_z = 'ㄆ'
    case('m');  i_z = 'ㄇ'
    case('f');  i_z = 'ㄈ'
    case('d');  i_z = 'ㄉ'
    case('t');  i_z = 'ㄊ'
    case('n');  i_z = 'ㄋ'
    case('l');  i_z = 'ㄌ'
    case('g');  i_z = 'ㄍ'
    case('k');  i_z = 'ㄎ'
    case('ng'); i_z = 'ㄫ'
    case('h');  i_z = 'ㄏ'
    case('j');  i_z = '廴'
    case('w');  i_z = '户'
    case('gw'); i_z = 'ㆼ'
    case('kw'); i_z = 'ㆽ'
    case('z');  if (pal) then; i_z = 'ㄐ'; else; i_z = 'ㄗ'; end if
    case('c');  if (pal) then; i_z = 'ㄑ'; else; i_z = 'ㄘ'; end if
    case('s');  if (pal) then; i_z = 'ㄒ'; else; i_z = 'ㄙ'; end if
    case default; i_z = ''
    end select

    !--- final
    select case(trim(finl))
    case('aa');   f_z = 'ㄚ'
    case('aai');  f_z = 'ㄞ'
    case('aau');  f_z = 'ㄠ'
    case('aam');  f_z = 'ㆰ'
    case('aan');  f_z = 'ㄢ'
    case('aang'); f_z = 'ㄤ'
    case('aap');  f_z = 'ㄚㆴ'
    case('aat');  f_z = 'ㄚㆵ'
    case('aak');  f_z = 'ㄚㆻ'
    case('ai');   f_z = 'ㄟ'
    case('au');   f_z = 'ㄡ'
    case('am');   f_z = 'ㆿㆬ'
    case('an');   f_z = 'ㄣ'
    case('ang');  f_z = 'ㄥ'
    case('ap');   f_z = 'ㆿㆴ'
    case('at');   f_z = 'ㆿㆵ'
    case('ak');   f_z = 'ㆿㆻ'
    case('e');    f_z = 'ㄝ'
    case('ei');   f_z = 'ㆤ'
    case('eng');  f_z = 'ㄝㄥ'
    case('ek');   f_z = 'ㄝㆻ'
    case('ep');   f_z = 'ㄝㆴ'
    case('et');   f_z = 'ㄝㆵ'
    case('eu');   f_z = 'ㆤㄨ'
    case('i');    f_z = 'ㄧ'
    case('iu');   f_z = 'ㄭ'
    case('im');   f_z = 'ㄧㆬ'
    case('in');   f_z = 'ㄧㄣ'
    case('ing');  f_z = 'ㄧㄥ'
    case('ip');   f_z = 'ㄧㆴ'
    case('it');   f_z = 'ㄧㆵ'
    case('ik');   f_z = 'ㄧㆻ'
    case('o');    f_z = 'ㄛ'
    case('oi');   f_z = 'ㄜ'
    case('ou');   f_z = 'ㆦ'
    case('on');   f_z = 'ㄛㄣ'
    case('ong');  f_z = 'ㆲ'
    case('ot');   f_z = 'ㄛㆵ'
    case('ok');   f_z = 'ㄛㆻ'
    case('u');    f_z = 'ㄨ'
    case('ui');   f_z = 'ㆨ'
    case('un');   f_z = 'ㄨㄣ'
    case('ung');  f_z = 'ㄨㄥ'
    case('ut');   f_z = 'ㄨㆵ'
    case('uk');   f_z = 'ㄨㆻ'
    case('oe');   f_z = 'ㆾ'
    case('eoi');  f_z = 'ㆾㄩ'
    case('eon');  f_z = 'ㆾㄣ'
    case('oeng'); f_z = 'ㆾㄥ'
    case('eot');  f_z = 'ㆾㆵ'
    case('oek');  f_z = 'ㆾㆻ'
    case('yu');   f_z = 'ㄩ'
    case('yun');  f_z = 'ㄩㄣ'
    case('yut');  f_z = 'ㄩㆵ'
    case('m');    f_z = 'ㆬ'
    case('ng');   f_z = 'ㆭ'
    case default; f_z = ''
    end select

    !--- tone mark
    select case(tone)
    case(1); t_z = ''
    case(2); t_z = 'ˊ'
    case(3); t_z = '˫'
    case(4); t_z = 'ˋ'
    case(5); t_z = '˜'
    case(6); t_z = '˫'
    case default; t_z = ''
    end select

    out = i_z // f_z // t_z
end subroutine to_zhuyin

end program yutyut_converter
