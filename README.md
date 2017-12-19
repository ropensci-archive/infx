
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- build with rmarkdown::render("README.Rmd") -->
infx
====

Access to [InfectX](http://www.infectx.ch)/[TargetInfectX](https://www.targetinfectx.ch) screening data from R. A browser-based view of the data is available [here](http://www.infectx.ch/databrowser).

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://img.shields.io/badge/lifecycle-experimental-orange.svg) [![Travis-CI Build Status](https://travis-ci.org/nbenn/infx.svg?branch=master)](https://travis-ci.org/nbenn/infx) [![Coverage status](https://codecov.io/gh/nbenn/infx/branch/master/graph/badge.svg)](https://codecov.io/github/nbenn/infx?branch=master)

Installation
------------

You can install infx from github with:

``` r
# install.packages("devtools")
devtools::install_github("nbenn/infx")
```

OpenBIS API
-----------

Documentation to the full JSON-RPC API can be accessed [here](https://wiki-bsse.ethz.ch/display/openBISDoc1304/openBIS+JSON+API). The available methods are grouped as:

-   [IGeneralInformationService](http://svnsis.ethz.ch/doc/openbis/13.04.0/ch/systemsx/cisd/openbis/generic/shared/api/v1/IGeneralInformationService.html)
    -   \[ \] ~~filterDataSetsVisibleToUser~~: Returns a filtered list of allDataSets containing those data sets which are visible to userId.
    -   \[ \] ~~filterExperimentsVisibleToUser~~: Returns a filtered list of allExperiments containing those experiments which are visible to userId.
    -   \[ \] ~~filterSamplesVisibleToUser~~: Returns a filtered list of allSamples containing those samples which are visible to userId.
    -   \[x\] getDataSetMetaData: Returns meta data for all specified data sets.
    -   \[x\] getDataSetMetaData: Returns meta data for all specified data sets.
    -   \[ \] getDataStoreBaseURLs: Returns the download URL for the data store of specified data sets.
    -   \[ \] getDefaultPutDataStoreBaseURL: Returns the URL for the default data store server for this openBIS AS.
    -   \[ \] getMaterialByCodes: Returns the materials with specified identifiers (i.e.
    -   \[ \] getMetaproject: Returns all entities tagged with given metaproject.
    -   \[ \] ~~getMetaprojectOnBehalfOfUser~~: Returns all entities tagged with given metaproject for specified user.
    -   \[ \] getVocabularyTermsMap: Deprecated.  Please use listVocabularies(String) instead.
    -   \[x\] isSessionActive: Returns true if session with the specified token is still active, false otherwise.
    -   \[ \] listAttachmentsForExperiment: Lists attachments of specified experiment.
    -   \[ \] listAttachmentsForProject: Lists attachments of specified project.
    -   \[ \] listAttachmentsForSample: Lists attachments of specified sample.
    -   \[x\] listDataSets: Return all data sets attached to the given samples.
    -   \[x\] listDataSets: Return all data sets attached to the given samples with connections.
    -   \[x\] listDataSetsForExperiments: Return all data sets attached to the given experiments with connections.
    -   \[ \] ~~listDataSetsForExperimentsOnBehalfOfUser~~: Return all data sets attached to the given experiments with connections that the user userId is allowed to see.
    -   \[x\] listDataSetsForSample: Return the data sets attached to the specified sample, optionally including child samples.
    -   \[ \] ~~listDataSetsOnBehalfOfUser~~: Return all data sets attached to the given samples with connections that the user userId is allowed to see.
    -   \[x\] listDataSetTypes: Returns all data set types.
    -   \[ \] listDataStores: Lists all DSS server registered this openBIS server instance.
    -   \[x\] listExperiments: Return all experiments of the given type that belong to the supplied projects.
    -   \[x\] listExperiments: Return all experiments matching a specified set of identifiers.
    -   \[ \] listExperimentsHavingDataSets: Return all experiments of the given type that belong to the supplied projects and have registered data sets.
    -   \[ \] listExperimentsHavingSamples: Return all experiments of the given type that belong to the supplied projects and have registered samles.
    -   \[x\] listExperimentTypes: Returns all experiment types.
    -   \[ \] listMetaprojects: Lists all metaprojects belonging to current user.
    -   \[ \] ~~listMetaprojectsOnBehalfOfUser~~: Lists all metaprojects belonging to specified user.
    -   \[ \] listNamedRoleSets: Returns all named role sets.
    -   \[x\] listProjects: Returns all available projects.
    -   \[ \] ~~listProjectsOnBehalfOfUser~~: Returns all available projects that a particular user is allowed to see.
    -   \[ \] listSamplesForExperiment: Return all samples that belong to the supplied experiment.
    -   \[ \] ~~listSamplesForExperimentOnBehalfOfUser~~: Return all samples that belong to the supplied experiment that are visible to user userId.
    -   \[ \] listSampleTypes: Returns all sample types.
    -   \[ \] listSpacesWithProjectsAndRoleAssignments: Returns all spaces of specified database instance enriched with their projects and role assignments.
    -   \[ \] listVocabularies: Returns all available vocabularies together with the contained terms.
    -   \[x\] logout: Logout the session with the specified session token.
    -   \[x\] searchForDataSets: Return all data sets matching specified search criteria.
    -   \[ \] ~~searchForDataSetsOnBehalfOfUser~~: Return all data sets matching specified search criteria and visible to user userId.
    -   \[ \] searchForExperiments: Returns all experiments matching specified search criteria.
    -   \[ \] searchForMaterials: Returns all material fulfilling specified search criteria.
    -   \[ \] searchForSamples: Return all samples that match the search criteria.
    -   \[ \] searchForSamples: Return all samples that match the search criteria.
    -   \[ \] ~~searchForSamplesOnBehalfOfUser~~: Return all samples that match the search criteria and that a particular user is allowed to see.
    -   \[ \] tryGetDataStoreBaseURL: Returns the download URL for the data store of specified data set or null if such data set does not exist.
    -   \[x\] tryToAuthenticateForAllServices: Tries to authenticate specified user with specified password.
-   [IGeneralInformationChangingService](http://svnsis.ethz.ch/doc/openbis/13.04.0/ch/systemsx/cisd/openbis/generic/shared/api/v1/IGeneralInformationChangingService.html)
    -   \[ \] addToMetaproject: Adds given entities to an existing metaproject.
    -   \[ \] addUnofficialVocabularyTerm: Adds new unofficial terms to a vocabulary starting from specified ordinal + 1.
    -   \[ \] addUnofficialVocabularyTerm: Deprecated.  Because the parameters refer to an internal openBIS class (TechID).
    -   \[ \] createMetaproject: Creates a new metaproject.
    -   \[ \] deleteMetaproject: Deletes an existing metaproject.
    -   \[ \] getWebAppSettings: Returns the persistent settings for a given custom web app.
    -   \[ \] removeFromMetaproject: Removes given entities from an existing metaproject.
    -   \[ \] setWebAppSettings: Sets the persistent settings for a given custom web app.
    -   \[ \] updateMetaproject: Updates an existing metaproject.
    -   \[ \] updateSampleProperties:
-   [IQueryApiServer](http://svnsis.ethz.ch/doc/openbis/13.04.0/ch/systemsx/cisd/openbis/plugin/query/shared/api/v1/IQueryApiServer.html)
    -   \[ \] createReportFromAggregationService: Executes the specified aggregation or ingestion service for the specified parameters and creates a report.
    -   \[ \] createReportFromDataSets: Creates for the specified data sets a report.
    -   \[ \] executeQuery: Executes specified query using specified parameter bindings.
    -   \[ \] listAggregationServices: Returns metadata for all aggregation and ingestion services.
    -   \[ \] listQueries: Lists all queries available for the user of the specified session.
    -   \[ \] listTableReportDescriptions: Returns meta data for all reporting plugins which deliver a table.
    -   \[x\] logout: Logout the session with the specified session token.
    -   \[ \] tryToAuthenticateAtQueryServer: Tries to authenticate specified user with specified password.
-   [IWebInformationService](http://svnsis.ethz.ch/doc/openbis/13.04.0/ch/systemsx/cisd/openbis/generic/shared/api/v1/IWebInformationService.html)
    -   \[ \] getSessionToken: Returns the server side session token for the current HTTP session.
-   [IDssServiceRpcGeneric](http://svnsis.ethz.ch/doc/openbis/13.04.0/ch/systemsx/cisd/openbis/dss/generic/shared/api/v1/IDssServiceRpcGeneric.html)
    -   \[ \] createReportFromAggregationService: Create the report from the specified aggregation service.
    -   \[ \] createReportFromDataSets: Creates for the specified data sets a report.
    -   \[ \] deleteSessionWorkspaceFile: Delete a file or directory in the session workspace.
    -   \[x\] getDownloadUrlForFileForDataSet: Returns an URL from which the requested file.
    -   \[x\] getDownloadUrlForFileForDataSet: Returns an URL from which the requested file of the specified data set can be downloaded.
    -   \[ \] getDownloadUrlForFileForDataSetWithTimeout: Returns an URL from which the requested file.
    -   \[ \] getDownloadUrlForFileForDataSetWithTimeout: Returns an URL from which the requested file of the specified data set can be downloaded.
    -   \[ \] getFileForDataSet: Deprecated.  use getDownloadUrlForFileForDataSet(String, DataSetFileDTO).
    -   \[ \] getFileForDataSet: Deprecated.  use getDownloadUrlForFileForDataSet(String, String, String).
    -   \[ \] getFileFromSessionWorkspace: Download a file from the user's session workspace.
    -   \[ \] getPathToDataSet: Get a path to the data set.
    -   \[ \] getValidationScript: Get the validation script for the specified data set type.
    -   \[ \] listAggregationServices: Returns metadata for all aggregation services.
    -   \[ \] listAllShares: Lists all shares.
    -   \[x\] listFilesForDataSet: Get an array of FileInfoDss objects that describe the file-system structure of the data set.
    -   \[x\] listFilesForDataSet: Get an array of FileInfoDss objects that describe the file-system structure of the data set.
    -   \[ \] listTableReportDescriptions: Returns meta data for all reporting plugins which deliver a table.
    -   \[ \] putDataSet: Upload a new data set to the DSS.
    -   \[ \] putFileSliceToSessionWorkspace: Upload a file slice to the user's session workspace.
    -   \[ \] putFileToSessionWorkspace: Upload a new file to the user's session workspace.
    -   \[ \] shuffleDataSet: Moves specified data set to specified share.
-   [IScreeningApiServer](http://svnsis.ethz.ch/doc/openbis/13.04.0/ch/systemsx/cisd/openbis/plugin/screening/shared/api/v1/IScreeningApiServer.html)
    -   \[x\] getDatasetIdentifiers: Converts a given list of dataset codes to dataset identifiers.
    -   \[ \] getExperimentImageMetadata: Returns aggregated metadata for all images/plates within one experiment.
    -   \[ \] getPlateMetadataList: Fetches the contents of a given list of plates.
    -   \[x\] getPlateSample: For a given plateIdentifier, return the corresponding Sample.
    -   \[ \] getWellSample: For a given wellIdentifier, return the corresponding Sample including properties.
    -   \[ \] listAvailableFeatureCodes: Groups the specified objects by a data store code and calls IDssServiceRpcScreening.listAvailableFeatureCodes(String, List) method for each group of objects on appropriate data store server.
    -   \[ \] listAvailableFeatures: Groups the specified objects by a data store code and calls IDssServiceRpcScreening.listAvailableFeatures(String, List) method for each group of objects on appropriate data store server.
    -   \[ \] listAvailableImageRepresentationFormats: Groups the specified objects by a data store code and calls IDssServiceRpcScreening.listAvailableImageRepresentationFormats(String, List) method for each group of objects on appropriate data store server.
    -   \[x\] listExperiments: Return the list of all visible experiments, along with their hierarchical context (space, project).
    -   \[x\] listExperiments: Return the list of all experiments visible to user userId, along with their hierarchical context (space, project).
    -   \[ \] listFeatureVectorDatasets: For a given set of plates (given by space / plate bar code), provide the list of all data sets containing feature vectors for each of these plates.
    -   \[x\] listImageDatasets: For a given set of plates provide the list of all data sets containing images for each of these plates.
    -   \[ \] listImageMetadata: Groups the specified objects by a data store code and calls IDssServiceRpcScreening.listImageMetadata(String, List) method for each group of objects on appropriate data store server.
    -   \[ \] listPlateMaterialMapping: For a given list of plates, return the mapping of plate wells to materials contained in each well.
    -   \[x\] listPlates: Return the list of all visible plates assigned to any experiment, along with their hierarchical context (space, project, experiment).
    -   \[x\] listPlates: Return the list of all plates assigned to the given experiment.
    -   \[ \] listPlateWells: For the given experimentIdentifier, find all plate locations that are connected to the specified materialIdentifier.
    -   \[ \] listPlateWells: For the given materialIdentifier, find all plate locations that are connected to it.
    -   \[ \] listPlateWells: For the given plateIdentifier find all wells that are connected to it.
    -   \[x\] listRawImageDatasets: For a given set of plates provide the list of all data sets containing raw images for each of these plates.
    -   \[x\] listSegmentationImageDatasets: For a given set of plates provide the list of all data sets containing segmentation images for each of these plates.
    -   \[ \] loadFeatures: Groups the specified objects by a data store code and calls IDssServiceRpcScreening.loadFeatures(String, List, List) method for each group of objects on appropriate data store server.
    -   \[ \] loadFeaturesForDatasetWellReferences: Groups the specified objects by a data store code and calls IDssServiceRpcScreening.loadFeaturesForDatasetWellReferences(String, List, List) method for each group of objects on appropriate data store server.
    -   \[ \] loadImagesBase64: Groups the specified objects by a data store code and calls IDssServiceRpcScreening.loadImagesBase64(String, List) method for each group of objects on appropriate data store server.
    -   \[ \] loadImagesBase64: Groups the specified objects by a data store code and calls IDssServiceRpcScreening.loadImagesBase64(String, List, boolean) method for each group of objects on appropriate data store server.
    -   \[ \] loadImagesBase64: Groups the specified objects by a data store code and calls IDssServiceRpcScreening.loadImagesBase64(String, List, IImageRepresentationFormatSelectionCriterion...) method for each group of objects on appropriate data store server.
    -   \[ \] loadImagesBase64: Groups the specified objects by a data store code and calls IDssServiceRpcScreening.loadImagesBase64(String, List, ImageRepresentationFormat) method for each group of objects on appropriate data store server.
    -   \[ \] loadImagesBase64: Groups the specified objects by a data store code and calls IDssServiceRpcScreening.loadImagesBase64(String, List, ImageSize) method for each group of objects on appropriate data store server.
    -   \[ \] loadImagesBase64: Groups the specified objects by a data store code and calls IDssServiceRpcScreening.loadImagesBase64(String, List, LoadImageConfiguration) method for each group of objects on appropriate data store server.
    -   \[ \] loadPhysicalThumbnailsBase64: Groups the specified objects by a data store code and calls IDssServiceRpcScreening.loadPhysicalThumbnailsBase64(String, List, ImageRepresentationFormat) method for each group of objects on appropriate data store server.
    -   \[x\] loadThumbnailImagesBase64: Groups the specified objects by a data store code and calls IDssServiceRpcScreening.loadThumbnailImagesBase64(String, List) method for each group of objects on appropriate data store server.
    -   \[ \] logoutScreening: Logout the session with the specified session token.
    -   \[ \] tryLoginScreening: Authenticates the user with a given password.
-   [IDssServiceRpcScreening](http://svnsis.ethz.ch/doc/openbis/13.04.0/ch/systemsx/cisd/openbis/dss/screening/shared/api/v1/IDssServiceRpcScreening.html)
    -   \[ \] getFeatureList: Return the feature codes of a specified feature list for a specified feature vector data set
    -   \[ \] getImageTransformerFactoryOrNull: Returns the transformer factory for the specified channel and the experiment to which the specified data sets belong.
    -   \[ \] listAvailableFeatureCodes: For a given set of feature vector data sets provide the list of all available features.
    -   \[ \] listAvailableFeatureNames: Deprecated.  Use listAvailableFeatureCodes(String, List) instead.
    -   \[ \] listAvailableFeatures: For a given set of feature vector data sets provide the list of all available features.
    -   \[ \] listAvailableImageRepresentationFormats: Return image representation formats available for the specified image data sets.
    -   \[ \] listImageMetadata: For a given set of image data sets, provide all image channels that have been acquired and the available (natural) image size(s).
    -   \[ \] listImageReferences: Lists microscopy image references for specified data set and channels.
    -   \[ \] listImageReferences: Lists microscopy image references for specified data set and channel.
    -   \[ \] listPlateImageReferences: Lists plate image references for specified data set, list of well positions and channels.
    -   \[ \] listPlateImageReferences: Lists plate image references for specified data set, list of well positions and channel.
    -   \[ \] loadFeatures: Conceptually, for a given list of data well references (i.e.
    -   \[ \] loadFeaturesForDatasetWellReferences: Conceptually, for a given list of dataset well references (i.e.
    -   \[ \] loadImages: Provide images for specified data set, list of well positions, channel, and optional thumb nail size.
    -   \[ \] loadImages: Provide images for specified microscopy data set, channel and optional thumb nail size.
    -   \[ \] loadImages: Provide images for a given list of image references (given by data set code, well position, channel and tile).
    -   \[ \] loadImages: Provide images for a given list of image references (specified by data set code, well position, channel and tile).
    -   \[ \] loadImages: Provides images for the specified list of image references (specified by data set code, well position, channel and tile) and image selection criteria.
    -   \[ \] loadImages: Provides images for the specified list of image references (specified by data set code, well position, channel and tile) and specified image representation format.
    -   \[ \] loadImages: Provide images (PNG encoded) for a given list of image references (given by data set code, well position, channel and tile).
    -   \[ \] loadImages: Provide images for a given list of image references (specified by data set code, well position, channel and tile).
    -   \[ \] loadImagesBase64: Returns the same images as loadImages(String, IDatasetIdentifier, List, String, ImageSize) but the result is a list of base64 encoded strings that contain the image data.
    -   \[ \] loadImagesBase64: Returns the same images as loadImages(String, IDatasetIdentifier, String, ImageSize) but the result is a list of base64 encoded strings that contain the image data.
    -   \[ \] loadImagesBase64: Returns the same images as loadImages(String, List) but the result is a list of base64 encoded strings that contain the image data.
    -   \[ \] loadImagesBase64: Returns the same images as loadImages(String, List, boolean) but the result is a list of base64 encoded strings that contain the image data.
    -   \[ \] loadImagesBase64: Returns the same images as loadImages(String, List, IImageRepresentationFormatSelectionCriterion...) but the result is a list of base64 encoded strings that contain the image data.
    -   \[ \] loadImagesBase64: Returns the same images as loadImages(String, List, ImageRepresentationFormat) but the result is a list of base64 encoded strings that contain the image data.
    -   \[ \] loadImagesBase64: Returns the same images as loadImages(String, List, ImageSize) but the result is a list of base64 encoded strings that contain the image data.
    -   \[ \] loadImagesBase64: Returns the same images as loadImages(String, List, ch.systemsx.cisd.openbis.plugin.screening.shared.api.v1.dto.LoadImageConfiguration) but the result is a list of base64 encoded strings that contain the image data.
    -   \[ \] loadPhysicalThumbnails: The fast method to provide registered thumbnail images (without calculating them) for the specified list of image references (specified by data set code, well position, channel and tile) and specified image representation format.
    -   \[ \] loadPhysicalThumbnailsBase64: /\*\* Returns the same images as loadPhysicalThumbnails(String, List, ImageRepresentationFormat) but the result is a list of base64 encoded strings that contain the image data.
    -   \[x\] loadThumbnailImages: Provide thumbnail images for specified microscopy data set.
    -   \[x\] loadThumbnailImages: Provide thumbnail images for a given list of image references (specified by data set code, well position, channel and tile).
    -   \[x\] loadThumbnailImagesBase64: Returns the same images as loadThumbnailImages(String, IDatasetIdentifier, List) but the result is a list of base64 encoded strings that contain the image data.
    -   \[x\] loadThumbnailImagesBase64: Returns the same images as loadThumbnailImages(String, List) but the result is a list of base64 encoded strings that contain the image data.
    -   \[ \] saveImageTransformerFactory: Saves the specified transformer factory for the specified channel of the specified data.
