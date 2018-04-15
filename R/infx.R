
#' API access to the InfectX data repository
#' 
#' The JSON-RPC based openBIS API can be conveniently accesses form R using
#' functionality provided with this package. The intended application is data
#' retrieval from the openBIS instance hosted by the InfectX high throughput
#' screening project. Therefore some aspects of the API geared more towards
#' data curation are currently not supported. However the basic infrastructure
#' for creating and issuing a request, as well as processing the response,
#' is exposed and missing functionality therefore could easily be added.
#'
#' Type information of JSON objects returned from the API is preserved as S3
#' class attribute and all JSON list structures additionally inherit from the
#' S3 class `json_class`. As such, a data set object retrieved form openBIS,
#' for example, will have two class attributes: `DataSet` and `json_class`.
#' Sets of `json_class` objects that are of the same sub-type can be
#' represented as `json_vec` objects of that sub-type. Several data set
#' objects therefore are assembled as list structure with S3 classes `DataSet`
#' and `json_vec`, where every entry in turn is an S3 object with types
#' `DataSet` and `json_class`. This approach was choses in order to not only
#' have available generic function dispatch on individual `json_class`
#' objects, but also on sets (or *vectors*) of `json_class` objects.
#' 
#' This documentation makes a distinction between objects in openBIS that exist
#' mainly for the purpose of organizing and grouping data and objects that
#' represent actual data resources. The most basic object in the organizational
#' hierarchy is that of a `Project`. Several `Experiment` objects may be
#' associated with a `Project` and `Sample` objects live in experiments. Given
#' the HTS-based context of InfectX data, samples typically represent
#' microtiter plates and individual plate wells. `Material` objects describe
#' agents applied to samples. Many of the InfectX screens are RNA interference-
#' based and therefore materials may be for example siRNA oligos or targeted
#' genes. Finally, samples are associated with `DataSet` objects that stand for
#' experimental measurements or data derived thereof.
#' 
#' Any type of data resource available in openBIS can be accessed as files
#' belonging to data sets. Due to the image-based nature of InfectX screens,
#' raw experimental data comes in the form of fluorescence microscopy imagery
#' which consequently constitutes the most basic form of data resource
#' available. It is therefore no surprise that image data receives special
#' treatment, allowing for more fine grained access and functionality that
#' helps with finding specific sub-sets of images. A further data resource
#' that comes with features similar to those of image data is termed feature
#' vector data sets. This is mostly tabular data with a single value
#' corresponding to an imaging site. This is typically used for image
#' acquisition meta data, summarized image analysis or quality control results.
#' 
#' @section JSON object handling:
#' Object structures as returned by openBIS can be instantiated using the
#' creator [json_class()]. This function takes an arbitrary set of key-value
#' pairs, followed by a class name and returns a list-based `json_class`
#' object. Existing list-based objects may be coerced to `json_class` using
#' [as_json_class()] where `@type` list entries are taken to be class types.
#' The inverse is achieved by calling [rm_json_class()] on a `json_class`
#' object or by calling [as_list()] and passing the `keep_asis` argument as
#' `FALSE`. `json_class` objects can be validated with [is_json_class()] which
#' is recursively called on any object inheriting from `json_class` in
#' [check_json_class()].
#' 
#' Similarly to `json_class` objects, a constructor for `json_vec` objects is
#' provided in the form if [json_vec()] and existing structures can be coerced
#' to `json_vec` by [as_json_vec()]. The validator function [is_json_vec()]
#' tests whether an object is a properly formed `json_vec` object and the
#' utility function [has_common_subclass()] tests whether the passed list
#' structure consists of `json_class` objects of the same sub-type. The inverse
#' of applying [as_json_vec()] to a list structure is achieved by passing a
#' `json_vec` object to [as_list()].
#' 
#' Several utility functions are provided that facilitate handling of
#' `json_class` and `json_vec` objects. [has_fields()] tests whether certain
#' named entries are present in a `json_class` object or in each member of a
#' `json_vec`. In order to extract the content of a field, [get_field()] can be
#' applied to `json_class` and `json_vec` objects. Analogously,
#' [has_subclass()] and [get_subclass()] test for and extract the original JSON
#' object type from `json_class` and `json_vec` objects. Finally,
#' [remove_null()] recursively removes empty fields (fields containing `NULL`)
#' from `json_class` and `json_vec` objects.
#' 
#' In addition to the mentioned utility functions, several base R generic
#' functions have `json_class` and `json_vec` specific methods implemented.
#' Combining several `json_class` objects using [base::c()] yields a `json_vec`
#' object, as does repeating objects using [base::rep()]. The same functions
#' can be applied to `json_vec` objects but this only checks for agreement in
#' sub-type. Custom sum-setting is provided as well, in order to retain class
#' attributes and replacement functions acting on `json_vec` objects make sure
#' that sub-types remain compatible. Recursive printing of both `json_class`
#' and `json_vec` objects is possible by calling [base::print()]. Recursion
#' depth, as well as printing length and width can be controlled via arguments,
#' as can fancy printing (colors and UTF box characters for visualizing tree
#' structures) be disabled.
#' 
#' @section Listing and searching for objects:
#' OpenBIS projects can be listed by calling [list_projects()] and experiments
#' are enumerated with [list_experiments()]. Two objects types are used to
#' represent experiments: `Experiment` and `ExperimentIdentifier`.
#' [exp_to_expid()] converts a set of `Experiment` objects to
#' `ExperimentIdentifier` and the inverse is possible by passing a set of
#' `ExperimentIdentifier` objects to [list_experiments()]. All available
#' experiments can be listed as `ExperimentIdentifier` objects using
#' [list_experiment_ids()] and all experiments for a set of projects are
#' enumerated by passing `Project` objects to [list_experiments()]. Experiments
#' have a type and all realized types can be listed with
#' [list_experiment_types()].
#' 
#' Experiments consist of samples which are listed by passing a set of
#' `Experiment` or `ExperimentIdentifier` objects to [list_samples()]. Samples
#' too have a type and all types are retrieved with [list_sample_types()].
#' Additional object types that are used to represent samples are plate and
#' well objects, including `Plate`, `PlateIdentifier`, `PlateMetadata`,
#' `WellIdentifier` and `WellMetadata`, all of which can be converted to
#' `Sample` objects by calling [list_samples()]. Plate objects can be listed
#' using [list_plates()], which can either return all available plate objects
#' or plates for a given set of experiments (passed as `Experiment` or
#' `ExperimentIdentifier` objects). Plate meta data, which also contains
#' associated well meta data is retrieved by [list_plate_metadata()] which can
#' act on plate objects (`Plate`, `PlateIdentifier` or `Sample`). Wells of a
#' plate are listed with [list_wells()] which too may be dispatched on plate
#' objects. Wells associated with a material object can be enumerated by
#' passing a set of `MaterialScreening`, `MaterialIdentifierScreening`,
#' `MaterialGeneric` or `MaterialIdentifierGeneric` to [list_wells()].
#' 
#' Data set objects represent the most diverse group of data-organizational
#' structures. Possibly types include `DataSet`, `DatasetIdentifier`,
#' `DatasetReference`, `ImageDatasetReference`, `MicroscopyImageReference`,
#' `PlateImageReference`, `FeatureVectorDatasetReference` and
#' `FeatureVectorDatasetWellReference`.
#' 
#' @section Downloading data:
#' The foo functions ...
#'
#' @docType package
#' @name infx
#' 
#' @import assertthat
NULL
