    FUNCTION SRC_READ(FilePath, tItemName, Record)
! * COPIED from SVN program
!
! Function that works like a READ but designed to
! work from the source repository.
! Primary purpose is to read a specific revision for
! subsequent use in a compare routine.
!
! Function returns false if unsuccessful
! (i.e. the requested item/revision is not there)
! in which case any Record is returned in the Record arg
!
! 02 DEC 2009 Peter Falson (jBASE)
!
    DEFFUN GETSRCTYPE()
    DEFFUN GIT_READ()
    DEFFUN SVN_READ()
    scType = GETSRCTYPE()
    BEGIN CASE
        CASE scType = 'GIT'
            RETURN GIT_READ(FilePath, tItemName, Record)
        CASE scType = 'SVN'
            RETURN SVN_READ(FilePath, tItemName, Record)
    END CASE
