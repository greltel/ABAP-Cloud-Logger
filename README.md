# ABAP-Cloud-Logger
ABAP Logger Following Clean Core Principles.ABAP Cloud Logger is a modern, lightweight, and Clean Core-compliant logging library for SAP S/4HANA and SAP BTP ABAP Environment.
It acts as a fluent wrapper around the standard class `CL_BALI_LOG`, simplifying the Application Log API while ensuring strict adherence to **ABAP Cloud** development standards.

# Prerequisites

* SAP S/4HANA 2021 (or higher) OR SAP BTP ABAP Environment.

## License
This project is licensed under the [MIT License](https://github.com/greltel/ABAP-Cloud-Logger/blob/main/LICENSE).

## Contributors-Developers
The repository was created by [George Drakos](https://www.linkedin.com/in/george-drakos/).

## Motivation for Creating the Repository

Logging is a tool I rely on almost every day while working with ABAP. I wanted to create a version that is not only simple and effective but also fully compatible with the ABAP Cloud environment. 
The goal is to provide an easy-to-use logger that fits naturally into cloud-ready development practices and can be integrated seamlessly into modern projects.

## Usage Examples

### 1. Initialization
To start logging, get an instance of the logger by providing your Application Log Object and Subobject (defined in `SLG0` or via Cloud API).

```abap
DATA(lo_logger) = zcl_cloud_logger=>get_instance(
  iv_object    = 'Z_MY_OBJECT'
  iv_subobject = 'Z_MY_SUBOBJECT').
```

### 2. Exception Add

```abap
TRY.
    " Your business logic here
    DATA(result) = 100 / 0.

  CATCH cx_sy_zerodivide INTO DATA(lx_error).
    " Pass the exception object to the logger
    lo_logger->log_exception_add( lx_error ).
ENDTRY.
```

### 3. String Add

```abap
lo_logger->log_message_add( iv_msgty = 'W'
                            iv_msgid = '00'
                            iv_msgno = '001'
                            iv_msgv1 = 'Message Add').
```
                            
### 4. Message Add

```abap
lo_logger->log_string_add( iv_string = 'String Add'
                           iv_msgty  = 'E'  ).
```
### 5. BAPIRET2 Structure and Table Add

```abap
lo_logger->log_bapiret2_table_add( VALUE #( ( ) ) ).

lo_logger->log_bapiret2_structure_add( VALUE #( ) ) .
```

### 6. Get Messages

```abap
DATA(lv_message_count)     = lo_logger->get_message_count( ).

DATA(lt_messages_bapiret2) = lo_logger->get_messages_as_bapiret2( ).

DATA(lt_messages)          = lo_logger->get_messages( ).

DATA(lt_messages_flat)     = lo_logger->get_messages_flat( ).

DATA(lt_messages_rap)      = lo_logger->get_messages_rap( ).
```

### 7. Functional Methods

```abap
data(lv_error_exists)   = lo_logger->log_contains_error( ).

data(lv_messages_exist) = lo_logger->log_contains_messages( ).

data(lv_warning_exists) = lo_logger->log_contains_warning( ).
```

### 8. Save Application Log

```abap
lo_logger->save_application_log( ).
```

## Design Goals/Features

* Install via [ABAPGit](http://abapgit.org)
* ABAP Cloud/Clean Core compatibility.Passed SCI check variant S4HANA_READINESS_2023
* Based on [CL_BALI_LOG](https://help.sap.com/docs/btp/sap-business-technology-platform/cl-bali-log-interface-if-bali-log) which is released for Cloud Development (could also use XCO_CP_BAL)
* Based on Multiton Design Pattern for efficient management of log instances.
