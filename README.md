# ✅ Status: Initial Release (v1.0.0)
[![Version](https://img.shields.io/badge/version-1.0.0-blue.svg)](https://github.com/greltel/ABAP-Point-Gate/releases)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://github.com/greltel/ABAP-Cloud-Logger/blob/main/LICENSE)
![ABAP 7.00+](https://img.shields.io/badge/ABAP-7.00%2B-brightgreen)
[![Code Statistics](https://img.shields.io/badge/CodeStatistics-abaplint-blue)](https://abaplint.app/stats/greltel/ABAP-Cloud-Logger)

# Table of contents
1. [ABAP Cloud Logger](#ABAP-Cloud-Logger)
2. [Prerequisites](#Prerequisites)
3. [License](#License)
4. [Contributors-Developers](#Contributors-Developers)
5. [Motivation for Creating the Repository](#Motivation-for-Creating-the-Repository)
6. [Usage Examples](#Usage-Examples)
7. [Design Goals-Features](#Design-Goals-Features)

# ABAP-Cloud-Logger
ABAP Logger Following Clean Core Principles.ABAP Cloud Logger is a modern, lightweight, and Clean Core-compliant logging library for SAP S/4HANA and SAP BTP ABAP Environment.
It acts as a fluent wrapper around the standard class `CL_BALI_LOG`, simplifying the Application Log API while ensuring strict adherence to **ABAP Cloud** development standards.

# Prerequisites

* SAP S/4HANA 2021 (or higher) OR SAP BTP ABAP Environment.
* XCO library availability

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
DATA(lo_logger) = zcl_cloud_logger=>get_instance( iv_object    = 'Z_CLOUD_LOG'
                                                  iv_subobject = 'Z_CLOUD_LOG' ).
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

### 3. Message Add

```abap
lo_logger->log_message_add( iv_symsg = VALUE #( msgty = 'W'
                                                msgid = 'CL'
                                                msgno = '000'
                                                msgv1 = 'Test Message' ) ).
```
                            
### 4. String Add

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
DATA(lv_error_exists)   = lo_logger->log_contains_error( ).

DATA(lv_messages_exist) = lo_logger->log_contains_messages( ).

DATA(lv_warning_exists) = lo_logger->log_contains_warning( ).

DATA(lv_is_empty)       = lo_logger->log_is_empty( ).
```

### 8. Get Log Handle

```abap
DATA(lv_handle)         = lo_logger->get_handle( ).

DATA(lv_log_handle)     = lo_logger->get_log_handle( ).
```

### 9. Save Application Log

```abap
lo_logger->save_application_log( ).
```

### 10. Search for a Specific Message

```abap
data(lv_specific_message_exists) = lo_logger->search_message( im_search = VALUE #( msgid = '00' ) ).
```

### 11. Merge Logs

```abap
"Get a New Log Instance
DATA(lo_new_logger) = zcl_cloud_logger=>get_instance( iv_object    = 'Z_MY_OBJECT_NEW'
                                                      iv_subobject = 'Z_MY_SUBOBJECT_NEW' ).

"Add Message
lo_new_logger->log_string_add( iv_string = 'New Logger String'
                               iv_msgty  = 'E'  ).

"Merge with Previous Log
lo_logger->merge_logs( lo_new_logger ).
```

### 12. Reset Log

```abap
lo_logger->reset_appl_log( ).
```

## Design Goals-Features

* Install via [ABAPGit](http://abapgit.org)
* ABAP Cloud/Clean Core compatibility.Passed SCI check variant S4HANA_READINESS_2023 and SAP_CP_READINESS
* Based on [CL_BALI_LOG](https://help.sap.com/docs/btp/sap-business-technology-platform/cl-bali-log-interface-if-bali-log) which is released for Cloud Development (could also use XCO_CP_BAL)
* Based on Multiton Design Pattern for efficient management of log instances
* Unit Tested
