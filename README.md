# ABAP-Cloud-Logger
ABAP Logger Following Clean Core Principles.ABAP Cloud Logger is a modern, lightweight, and Clean Core-compliant logging library for SAP S/4HANA and SAP BTP ABAP Environment.
It acts as a fluent wrapper around the standard class `CL_BALI_LOG`, simplifying the Application Log API while ensuring strict adherence to **ABAP Cloud** development standards.

# Prerequisites

* SAP S/4HANA 2022 (or higher) OR SAP BTP ABAP Environment.

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

## Design Goals/Features

* Install via [ABAPGit](http://abapgit.org)
* ABAP Cloud/Clean Core compatibility.Passed SCI check variant S4HANA_READINESS_2023
* Based on [CL_BALI_LOG](https://help.sap.com/docs/btp/sap-business-technology-platform/cl-bali-log-interface-if-bali-log) which is released for Cloud Development (could also use XCO_CP_BAL)
* Based on Multiton Design Pattern for efficient management of log instances.
