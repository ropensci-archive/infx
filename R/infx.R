
#' API access to the InfectX data repository
#' 
#' The JSON-RPC based [openBIS](https://openbis.elnlims.ch) API can be
#' conveniently queried form R using functionality provided with this package.
#' While focused on retrieval of data from the
#' [InfectX](http://www.infectx.ch) and
#' [TargetInfectX](https://www.targetinfectx.ch) high throughput screening
#' projects, any openBIS instance that supports v1 of the JSON-RPC API can be
#' accessed. Some parts of the API, geared more towards data curation are
#' currently not supported. For more infomartion on what API functions are
#' available, have a look at the
#' [openBIS API vignette](../doc/openbis-api.html). The basic infrastructure
#' for creating and executing a request, as well as processing the response, is
#' exposed and missing functionality can easily be added.
#'
#' Type information of JSON objects returned from the API is preserved as S3
#' class attribute and all JSON list structures additionally inherit from the
#' S3 class `json_class`. As such, a `foobar` object retrieved form openBIS,
#' will have two class attributes: `foobar` and `json_class`. Sets of
#' `json_class` objects that are of the same sub-type can be represented as
#' `json_vec` objects of that sub-type. Several data set objects therefore are
#' assembled as list structure with S3 classes `foobar` and `json_vec`, where
#' every entry in turn is an S3 object with types `foobar` and `json_class`.
#' 
#' ```
#' examp <- json_vec(
#'   json_class(a = "foo", class = "foobar"),
#'   json_class(a = "bar", class = "foobar")
#' )
#' str(examp)
#' 
#' #> List of 2
#' #>  $ :List of 1
#' #>   ..$ a: chr "foo"
#' #>   ..- attr(*, "class")= chr [1:2] "foobar" "json_class"
#' #>  $ :List of 1
#' #>   ..$ a: chr "bar"
#' #>   ..- attr(*, "class")= chr [1:2] "foobar" "json_class"
#' #>  - attr(*, "class")= chr [1:2] "foobar" "json_vec"
#' ```
#' 
#' Such an approach was chosen in order to not only have available generic
#' function dispatch on individual `json_class` objects, but also on sets (or
#' *vectors*) of `json_class` objects. For more information on working with
#' `json_class` and `json_vec` objects refer to the
#' [section on JSON objects](#json-object-handling) and
#' [JSON object vignette](../doc/json-class.html).
#' 
#' This documentation makes a distinction between objects in openBIS that exist
#' mainly for the purpose of organizing/grouping data and objects that
#' represent actual data resources. The most basic object in the organizational
#' hierarchy is that of a `Project`. Several `Experiment` objects may be
#' associated with a `Project` and `Sample` objects live in experiments. Given
#' the HTS-based context of InfectX data, samples typically represent
#' microtiter plates or individual plate wells. `Material` objects describe
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
#' @section General comments:
#' A login token is required for any type of API call. Passing valid login
#' credentials to [login_openbis()] will return a string that can subsequently
#' be used for making API requests. Login tokens are invalidated by calling
#' [logout_openbis()] which is performed automatically upon garbage collection
#' of login tokens returned by [login_openbis()] with the `auto_disconnect`
#' switch set to `TRUE` (default). Validity of a login token can be checked
#' with [is_token_valid()].
#' 
#' All API requests are constructed by [make_requests()] (or for single
#' requests by the wrapper function [make_request()]), which helps with putting
#' together JSON-RPC requests and parses the returned JSON objects by calling
#' [process_json()]. Processing of JSON involves generation of `json_class`
#' and `json_vec` objects using `@type` information, as well as resolution of
#' `@id` references. While obviously a feature for reducing data transfer
#' overhead, this type of data deduplication has the down-side of yielding
#' objects that are no longer self-contained. If for example plate wells are
#' listed and each well contains an object referencing the associated plate,
#' only a single instance of this plate object will be retrieved as part of the
#' first well object and all subsequent well objects only contain a reference
#' to this plate object. Sub-setting this list of wells however might yield
#' well objects with broken references. To circumvent such issues, all
#' references are resolved by a call to [resolve_references()], initiated by
#' [process_json()].
#' 
#' As a side note: while created for and mainly tested with
#' [InfectX](https://infectx.biozentrum.unibas.ch/openbis) data, all API
#' methods can be used for accessing other openBIS instances as well.
#' Functions that issue API calls can all accept a `host_url` argument which
#' is forwarded to [api_url()] in [make_requests()] in order to create API
#' endpoint urls. Another publicly available openBIS instance is the
#' [demo](https://openbis.elnlims.ch/openbis) offered by the openBIS
#' development team. It can be accessed with both user name and password
#' `test_observer` both via a browser or by passing
#' `https://openbis.elnlims.ch` as `host_url` to methods which initiate API
#' calls.
#' 
#' After being assembled by [make_requests()], requests are executed by
#' [do_requests_serial()] or [do_requests_parallel()], depending on whether
#' several API calls are constructed at the same time. The argument `n_con`
#' controls the degree of parallelism and if set to `1`, forces serial
#' execution even in cases where several requests are being issued. Failed
#' requests can be automatically repeated to provide additional stability by
#' setting the `n_try` argument to a value larger than `1` (default is `2`).
#' For more information on how to add further functionality using
#' [make_requests()] and [do_requests_serial()]/[do_requests_parallel()],
#' refer to the [openBIS API vignette](../doc/openbis-api.html). 
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
#' provided in the form of [json_vec()] and existing structures can be coerced
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
#' structures).
#' 
#' @section Listing and searching for objects:
#' OpenBIS projects can be listed by calling [list_projects()] and experiments
#' are enumerated with [list_experiments()]. Two objects types are used for
#' representing experiments: `Experiment` and `ExperimentIdentifier`.
#' [as_experiment_id()] converts a set of `Experiment` objects to
#' `ExperimentIdentifier` (requires no API call) and the inverse is possible
#' by passing a set of `ExperimentIdentifier` objects to [list_experiments()]
#' (does require an API call). All available experiments can be listed as
#' `ExperimentIdentifier` objects using [list_experiment_ids()] and all
#' experiments for a set of projects are enumerated by passing `Project`
#' objects to [list_experiments()]. Experiments have a type and all realized
#' types can be listed with [list_experiment_types()].
#' 
#' Experiments consist of samples which can be listed by passing a set of
#' `Experiment` or `ExperimentIdentifier` objects to [list_samples()]. Samples
#' too have a type and all types are retrieved by calling
#' [list_sample_types()]. Additional object types that are used to represent
#' samples are plate and well objects, including `Plate`, `PlateIdentifier`,
#' `PlateMetadata`, `WellIdentifier` and `WellMetadata`, all of which can be
#' converted to `Sample` objects by calling [list_samples()]. Plate objects
#' can be listed using [list_plates()], which can either return all available
#' plate objects or plates for a given set of experiments (passed as
#' `Experiment` or `ExperimentIdentifier` objects). Plate meta data, which
#' also contains associated well meta data is retrieved by
#' [list_plate_metadata()] which can act on plate objects (`Plate`,
#' `PlateIdentifier` or `Sample`). Wells of a plate are listed with
#' [list_wells()] which too may be dispatched on plate objects. Wells
#' associated with a material object can be enumerated by passing a set of
#' `MaterialScreening`, `MaterialIdentifierScreening`, `MaterialGeneric` or
#' `MaterialIdentifierGeneric` to [list_wells()].
#' 
#' Data set objects represent the most diverse group of data-organizational
#' structures. Possible types include `DataSet`, `DatasetIdentifier`,
#' `DatasetReference`, `ImageDatasetReference`, `MicroscopyImageReference`,
#' `PlateImageReference`, `FeatureVectorDatasetReference` and
#' `FeatureVectorDatasetWellReference`. Full `DataSet` objects are returned by
#' [list_datasets()], either for a set of plate samples, experiments or data
#' set codes (passed as character vector). [list_dataset_ids()] gives back
#' `DatasetIdentifier` objects, either for a set of `DataSet` objects or data
#' set codes (again passed as character vector). The remaining data set types
#' are generated by [list_references()], and return type depends on input
#' arguments.
#' 
#' Whenever [list_references()] is dispatched on objects identifying a plate
#' sample (`Plate`, `PlateIdentifier`, `PlateMetadata` or `Sample`), a `type`
#' argument is available, which can be any of `raw`, `segmentation` or
#' `feature`. Depending on `type`, `ImageDatasetReference` or
#' `FeatureVectorDatasetReference` objects are returned. The former type of
#' objects represent plate-wise image data sets (either for raw images or
#' segmentation masks) while the latter type references feature vector data
#' sets.
#' 
#' Dispatch of [list_references()] is also possible on objects identifying
#' data sets and again the return type depends on further arguments. If
#' imaging channels are specified as `channels` argument, but not specific
#' wells are selected, `MicroscopyImageReference` objects are retrieved,
#' representing a plate-wide raw imaging data set per imaging site and imaging
#' channel. If in addition to imaging channels, wells are specified
#' (`WellPosition` objects, e.g. created by [well_pos()], passed as `wells`
#' argument), the return type changes to `PlateImageReference`. Such objects
#' precisely reference an image, by encoding imaging channel, imaging site,
#' well position and pate-wise imaging data set.
#' 
#' Finally, [list_references()] can be dispatched on material objects,
#' including `MaterialGeneric`, `MaterialScreening`,
#' `MaterialIdentifierGeneric` and `MaterialIdentifierScreening`, in which case
#' `PlateWellReferenceWithDatasets` objects are returned. While themselves
#' not representing data sets, `PlateWellReferenceWithDatasets` contain all
#' respective `ImageDatasetReference` and `FeatureVectorDatasetReference`
#' objects.
#' 
#' @section Search for objects:
#' Instead of enumerating objects using the various `list_*()` functions,
#' search queries can be constructed and run against openBIS. A search query
#' consists of a possibly nested `SearchCriteria` object as instantiated by
#' [search_criteria()] and is executed by calling [search_openbis()].
#' `SearchCriteria` objects are composed of a set of match clauses (see
#' [property_clause()], [any_property_clause()], [any_field_clause()],
#' [attribute_clause()] and [time_attribute_clause()]) which are combined by
#' an operator (either `any` or `all`).
#' 
#' Additionally, a single `SearchSubCriteria` may be attached to every
#' `SearchCriteria` object which in turn consists of a `SearchCriteria` and an
#' object type to which this search criteria object is applied to. In the call
#' to [search_openbis()] a target type has to be specified as `target_object`
#' argument (default is `data_set` and possible alternatives are `experiment`,
#' `material` as well as `sample`) to indicate what object type the search is
#' targeted at.
#' 
#' @section Downloading data:
#' As mentioned earlier, there are three types of data resources that can be
#' downloaded: files, images and feature vector data. File access is the most
#' basic method and any type of data (including images and feature data) is
#' available via this route. Accessing images and feature data using
#' specialized interfaces however simplifies and makes possibly more specific
#' data access.
#' 
#' Files can be listed for any object representing a data set as well as for a
#' character vector of data set codes using [list_files()]. An object type,
#' specialized for referencing files in a data set is available as
#' `DataSetFileDTO` can also be passed to [list_files()]. This is useful
#' whenever only a subset of files within a data set, contained in a folder,
#' are of interest. In any case, [list_files()] returns a set of
#' `FileInfoDssDTO` objects. As no data set information is encoded in
#' `FileInfoDssDTO` objects, [list_files()] saves data set codes as `data_set`
#' attributes with each object. Download of files is done using
#' [fetch_files()], which requires for every requested file, the data set code
#' and file path. This information can be passed as separate character vectors,
#' `DataSetFileDTO` objects or `FileInfoDssDTO` objects with data set
#' information passed separately as character vector or as `data_set`
#' attribute with each object. Furthermore data set membership information can
#' be passed as any type of data set object and if no file paths are
#' specified, all available files for the given data sets are retrieved.
#' 
#' [fetch_files()] internally creates download urls by calling
#' [list_download_urls()] and uses [do_requests_serial()] or
#' [do_requests_parallel()] to execute the downloads. Whether downloads are
#' performed in serial or parallel fashion can be controlled using the `n_con`
#' argument. Additionally a function may be passed to [fetch_files()] as
#' `reader` argument which will be called on each downloaded file.
#' 
#' Images are retrieved using [fetch_images()]. If dispatch occurs on general
#' purpose data set objects, including `DatasetIdentifier`, `DatasetReference`
#' or `ImageDatasetReference`, further arguments for identifying images are
#' passed as `channels` and `well_positions`. As `MicroscopyImageReference`
#' objects already contain channel information, only well positions are needed
#' in order to specify images. Somewhat surprisingly, image tile information
#' which is also part of `MicroscopyImageReference` objects is disregarded and
#' images are fetched for entire wells. Data sets that are connected to wells
#' and not plates can be passed to [fetch_images()] without additionally
#' specifying well locations. Images can be scaled down to smaller sizes either
#' by setting the `thumbnails` argument to `TRUE` (only possible for data sets
#' connected to wells instead of plates, as the corresponding API call does
#' not support selecting wells) or by passing an `ImageSize` object as
#' `image_size` argument, in which case returned images will be scaled to fit
#' within the box specified by the `ImageSize` object, while retaining the
#' original aspect ratio.
#' 
#' `PlateImageReference` objects most precisely reference images, as they
#' contain data set, well location, site location and channel information. If
#' a set of `PlateImageReference` objects is passed to [fetch_images()], image
#' size can be set using the `thumbnails` or `image_size` arguments and image
#' file type can be forced to png using the `force_png` switch. Most
#' fine-grained control over the returned images is achieved by using
#' `ImageRepresentationFormat` objects. Pre-defined format objects can be
#' retrieved per data set by calling [list_image_metadata()] with `type` set to
#' `format`. General image meta data, such as tile layout and channel
#' information is returned by [list_image_metadata()] if the `type` argument
#' is left at default value `metadata`.
#' 
#' Two types of objects are central to specifying feature data sets:
#' `FeatureVectorDatasetReference` and `FeatureVectorDatasetWellReference`
#' where the former object type references feature data for an entire plate and
#' the latter for individual wells on a plate. Both object types may be passed
#' to [fetch_features()] which returns objects of type `FeatureVectorDataset`
#' whenever a full plate is requested and `FeatureVectorWithDescription` for
#' individual wells. Features are selected by passing a character vector of
#' feature codes as `feature_codes` argument, the possible values of which
#' can be enumerated for a feature vector data set by calling
#' [list_feature_codes()] or by extracting the `code` entries from
#' `FeatureInformation` objects as retrieved by [list_features()]. In case the
#' `feature_codes` argument is left at default value (`NA`), all available
#' features are returned by [fetch_features()].
#'
#' @docType package
#' @name infx
#' 
#' @importFrom assertthat assert_that is.string is.count
NULL

#' Set object attribute
#' 
#' Inspired by [stats::setNames], this function assigns an attribute to an
#' object and returns the object.
#' 
#' @param object Object to which the attribute is assigned.
#' @param attribute The attribute itself.
#' @param attr_name Attribute name.
#' 
#' @noRd
#' 
set_attr <- function(object, attribute, attr_name) {
  assert_that(is.string(attr_name))
  attr(object, attr_name) <- attribute
  object
}
