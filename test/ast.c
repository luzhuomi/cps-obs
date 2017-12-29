typedef long int __ssize_t;
typedef __ssize_t ssize_t;
typedef ssize_t Py_ssize_t;
typedef struct _object {
            Py_ssize_t ob_refcnt; 
        } PyObject;

static void ast_error_finish(const char * filename)
{
    PyObject * type, * value, * tback, * errstr, * loc, * tmp;
    long lineno;
    PyErr_Occurred() ? (void) 0 : __assert_fail("PyErr_Occurred()",
                                                "ast_orig.c",
                                                85,
                                                __PRETTY_FUNCTION__);
    if (!PyErr_ExceptionMatches(PyExc_SyntaxError))
    {
        return;
    }
    PyErr_Fetch(&type, &value, &tback);
    errstr = PyTuple_GetItem(value, 0);
    if (!errstr)
    {
        return;
    }
    ((PyObject *) errstr)->ob_refcnt++;
    lineno = PyInt_AsLong(PyTuple_GetItem(value, 1));
    if (lineno == -1)
    {
        do
        {
            if (--((PyObject *) errstr)->ob_refcnt != 0)
            {
                ;
            }
            else
            {
                (*((PyObject *) (PyObject *) errstr)->ob_type->tp_dealloc)((PyObject *) (PyObject *) errstr);
            }
        }
        while (0);
        return;
    }
    do
    {
        if (--((PyObject *) value)->ob_refcnt != 0)
        {
            ;
        }
        else
        {
            (*((PyObject *) (PyObject *) value)->ob_type->tp_dealloc)((PyObject *) (PyObject *) value);
        }
    }
    while (0);
    loc = PyErr_ProgramText(filename, lineno);
    if (!loc)
    {
      ((PyObject *) (&_Py_NoneStruct))->ob_refcnt++;
        loc = &_Py_NoneStruct;
    }
    tmp = Py_BuildValue("(zlOO)",
                        filename,
                        lineno,
                        &_Py_NoneStruct,
                        loc);
    do
    {
        if (--((PyObject *) loc)->ob_refcnt != 0)
        {
            ;
        }
        else
        {
            (*((PyObject *) (PyObject *) loc)->ob_type->tp_dealloc)((PyObject *) (PyObject *) loc);
        }
    }
    while (0);
    if (!tmp)
    {
        do
        {
            if (--((PyObject *) errstr)->ob_refcnt != 0)
            {
                ;
            }
            else
            {
                (*((PyObject *) (PyObject *) errstr)->ob_type->tp_dealloc)((PyObject *) (PyObject *) errstr);
            }
        }
        while (0);
        return;
    }
    value = PyTuple_Pack(2, errstr, tmp);
    do
    {
        if (--((PyObject *) errstr)->ob_refcnt != 0)
        {
            ;
        }
        else
        {
            (*((PyObject *) (PyObject *) errstr)->ob_type->tp_dealloc)((PyObject *) (PyObject *) errstr);
        }
    }
    while (0);
    do
    {
        if (--((PyObject *) tmp)->ob_refcnt != 0)
        {
            ;
        }
        else
        {
            (*((PyObject *) (PyObject *) tmp)->ob_type->tp_dealloc)((PyObject *) (PyObject *) tmp);
        }
    }
    while (0);
    if (!value)
    {
        return;
    }
    PyErr_Restore(type, value, tback);
}
