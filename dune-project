(lang dune 3.6)

;; dune-project is auto-generated, see bin/awsm_build.ml

(name awsm)

; generating the opam files pollutes the top-level directory too much
; try doing without, here, until this is merged
; https://github.com/ocaml/dune/issues/6205

(generate_opam_files true)

(license MIT)

(authors "Solvuu")

(maintainers "Ashish Agarwal <ashish@solvuu.com>")

(homepage "https://github.com/solvuu/awsm")

(source
 (github solvuu/awsm))

(package
 (name awsm)
 (synopsis "AWS API base library")
 (depends
  awsm-codegen
  (base64
   (>= 3.1.0))
  (cohttp
   (>= 0.21.0))
  cryptokit
  xmlm
  zarith))

(package
 (name awsm-async)
 (synopsis "AWS API base library Async")
 (depends
  awsm
  cohttp-async
  (async
   (>= v0.11.0))
  async_ssl))

(package
 (name awsm-lwt)
 (synopsis "AWS API base library Lwt")
 (depends awsm lwt cohttp-lwt-unix))

(package
 (name awsm-codegen)
 (synopsis "AWS botocore code generator")
 (depends
  cohttp
  (core
   (< v0.16.0))
  core_unix
  ocamlformat
  ocamlgraph
  ppxlib
  (re
   (>= 1.7.2))
  (sedlex
   (>= 2.3))
  xmlm
  yojson))

(package
 (name awsm-unix)
 (synopsis "AWS API base library Unix"))

;; Packages above this line are base packages needed by all services.

;; The following SRP packages provide support for Cognito SRP.

(package
 (name awsm-srp)
 (synopsis "AWS API for SRP")
 (depends awsm))

(package
 (name awsm-srp-async)
 (synopsis "AWS Async API for SRP")
 (depends awsm-async awsm-cognito-idp-async awsm-srp))

;; Packages after this line each regard a specific service.

(package
 (name awsm-accessanalyzer)
 (synopsis "AWS API for Accessanalyzer")
 (depends awsm))

(package
 (name awsm-accessanalyzer-async)
 (synopsis "AWS Async API for Accessanalyzer")
 (depends awsm-async awsm-accessanalyzer))

(package
 (name awsm-account)
 (synopsis "AWS API for Account")
 (depends awsm))

(package
 (name awsm-account-async)
 (synopsis "AWS Async API for Account")
 (depends awsm-async awsm-account))

(package
 (name awsm-acm)
 (synopsis "AWS API for Acm")
 (depends awsm))

(package
 (name awsm-acm-async)
 (synopsis "AWS Async API for Acm")
 (depends awsm-async awsm-acm))

(package
 (name awsm-acm-pca)
 (synopsis "AWS API for Acm-pca")
 (depends awsm))

(package
 (name awsm-acm-pca-async)
 (synopsis "AWS Async API for Acm-pca")
 (depends awsm-async awsm-acm-pca))

(package
 (name awsm-alexaforbusiness)
 (synopsis "AWS API for Alexaforbusiness")
 (depends awsm))

(package
 (name awsm-alexaforbusiness-async)
 (synopsis "AWS Async API for Alexaforbusiness")
 (depends awsm-async awsm-alexaforbusiness))

(package
 (name awsm-amp)
 (synopsis "AWS API for Amp")
 (depends awsm))

(package
 (name awsm-amp-async)
 (synopsis "AWS Async API for Amp")
 (depends awsm-async awsm-amp))

(package
 (name awsm-amplify)
 (synopsis "AWS API for Amplify")
 (depends awsm))

(package
 (name awsm-amplify-async)
 (synopsis "AWS Async API for Amplify")
 (depends awsm-async awsm-amplify))

(package
 (name awsm-amplifybackend)
 (synopsis "AWS API for Amplifybackend")
 (depends awsm))

(package
 (name awsm-amplifybackend-async)
 (synopsis "AWS Async API for Amplifybackend")
 (depends awsm-async awsm-amplifybackend))

(package
 (name awsm-amplifyuibuilder)
 (synopsis "AWS API for Amplifyuibuilder")
 (depends awsm))

(package
 (name awsm-amplifyuibuilder-async)
 (synopsis "AWS Async API for Amplifyuibuilder")
 (depends awsm-async awsm-amplifyuibuilder))

(package
 (name awsm-apigatewaymanagementapi)
 (synopsis "AWS API for Apigatewaymanagementapi")
 (depends awsm))

(package
 (name awsm-apigatewaymanagementapi-async)
 (synopsis "AWS Async API for Apigatewaymanagementapi")
 (depends awsm-async awsm-apigatewaymanagementapi))

(package
 (name awsm-appflow)
 (synopsis "AWS API for Appflow")
 (depends awsm))

(package
 (name awsm-appflow-async)
 (synopsis "AWS Async API for Appflow")
 (depends awsm-async awsm-appflow))

(package
 (name awsm-appintegrations)
 (synopsis "AWS API for Appintegrations")
 (depends awsm))

(package
 (name awsm-appintegrations-async)
 (synopsis "AWS Async API for Appintegrations")
 (depends awsm-async awsm-appintegrations))

(package
 (name awsm-application-autoscaling)
 (synopsis "AWS API for Application-autoscaling")
 (depends awsm))

(package
 (name awsm-application-autoscaling-async)
 (synopsis "AWS Async API for Application-autoscaling")
 (depends awsm-async awsm-application-autoscaling))

(package
 (name awsm-application-insights)
 (synopsis "AWS API for Application-insights")
 (depends awsm))

(package
 (name awsm-application-insights-async)
 (synopsis "AWS Async API for Application-insights")
 (depends awsm-async awsm-application-insights))

(package
 (name awsm-applicationcostprofiler)
 (synopsis "AWS API for Applicationcostprofiler")
 (depends awsm))

(package
 (name awsm-applicationcostprofiler-async)
 (synopsis "AWS Async API for Applicationcostprofiler")
 (depends awsm-async awsm-applicationcostprofiler))

(package
 (name awsm-appmesh)
 (synopsis "AWS API for Appmesh")
 (depends awsm))

(package
 (name awsm-appmesh-async)
 (synopsis "AWS Async API for Appmesh")
 (depends awsm-async awsm-appmesh))

(package
 (name awsm-apprunner)
 (synopsis "AWS API for Apprunner")
 (depends awsm))

(package
 (name awsm-apprunner-async)
 (synopsis "AWS Async API for Apprunner")
 (depends awsm-async awsm-apprunner))

(package
 (name awsm-appstream)
 (synopsis "AWS API for Appstream")
 (depends awsm))

(package
 (name awsm-appstream-async)
 (synopsis "AWS Async API for Appstream")
 (depends awsm-async awsm-appstream))

(package
 (name awsm-athena)
 (synopsis "AWS API for Athena")
 (depends awsm))

(package
 (name awsm-athena-async)
 (synopsis "AWS Async API for Athena")
 (depends awsm-async awsm-athena))

(package
 (name awsm-auditmanager)
 (synopsis "AWS API for Auditmanager")
 (depends awsm))

(package
 (name awsm-auditmanager-async)
 (synopsis "AWS Async API for Auditmanager")
 (depends awsm-async awsm-auditmanager))

(package
 (name awsm-autoscaling)
 (synopsis "AWS API for Autoscaling")
 (depends awsm))

(package
 (name awsm-autoscaling-async)
 (synopsis "AWS Async API for Autoscaling")
 (depends awsm-async awsm-autoscaling))

(package
 (name awsm-autoscaling-plans)
 (synopsis "AWS API for Autoscaling-plans")
 (depends awsm))

(package
 (name awsm-autoscaling-plans-async)
 (synopsis "AWS Async API for Autoscaling-plans")
 (depends awsm-async awsm-autoscaling-plans))

(package
 (name awsm-backup)
 (synopsis "AWS API for Backup")
 (depends awsm))

(package
 (name awsm-backup-async)
 (synopsis "AWS Async API for Backup")
 (depends awsm-async awsm-backup))

(package
 (name awsm-backup-gateway)
 (synopsis "AWS API for Backup-gateway")
 (depends awsm))

(package
 (name awsm-backup-gateway-async)
 (synopsis "AWS Async API for Backup-gateway")
 (depends awsm-async awsm-backup-gateway))

(package
 (name awsm-batch)
 (synopsis "AWS API for Batch")
 (depends awsm))

(package
 (name awsm-batch-async)
 (synopsis "AWS Async API for Batch")
 (depends awsm-async awsm-batch))

(package
 (name awsm-billingconductor)
 (synopsis "AWS API for Billingconductor")
 (depends awsm))

(package
 (name awsm-billingconductor-async)
 (synopsis "AWS Async API for Billingconductor")
 (depends awsm-async awsm-billingconductor))

(package
 (name awsm-braket)
 (synopsis "AWS API for Braket")
 (depends awsm))

(package
 (name awsm-braket-async)
 (synopsis "AWS Async API for Braket")
 (depends awsm-async awsm-braket))

(package
 (name awsm-budgets)
 (synopsis "AWS API for Budgets")
 (depends awsm))

(package
 (name awsm-budgets-async)
 (synopsis "AWS Async API for Budgets")
 (depends awsm-async awsm-budgets))

(package
 (name awsm-ce)
 (synopsis "AWS API for Ce")
 (depends awsm))

(package
 (name awsm-ce-async)
 (synopsis "AWS Async API for Ce")
 (depends awsm-async awsm-ce))

(package
 (name awsm-chime)
 (synopsis "AWS API for Chime")
 (depends awsm))

(package
 (name awsm-chime-async)
 (synopsis "AWS Async API for Chime")
 (depends awsm-async awsm-chime))

(package
 (name awsm-chime-sdk-identity)
 (synopsis "AWS API for Chime-sdk-identity")
 (depends awsm))

(package
 (name awsm-chime-sdk-identity-async)
 (synopsis "AWS Async API for Chime-sdk-identity")
 (depends awsm-async awsm-chime-sdk-identity))

(package
 (name awsm-chime-sdk-meetings)
 (synopsis "AWS API for Chime-sdk-meetings")
 (depends awsm))

(package
 (name awsm-chime-sdk-meetings-async)
 (synopsis "AWS Async API for Chime-sdk-meetings")
 (depends awsm-async awsm-chime-sdk-meetings))

(package
 (name awsm-chime-sdk-messaging)
 (synopsis "AWS API for Chime-sdk-messaging")
 (depends awsm))

(package
 (name awsm-chime-sdk-messaging-async)
 (synopsis "AWS Async API for Chime-sdk-messaging")
 (depends awsm-async awsm-chime-sdk-messaging))

(package
 (name awsm-cloud9)
 (synopsis "AWS API for Cloud9")
 (depends awsm))

(package
 (name awsm-cloud9-async)
 (synopsis "AWS Async API for Cloud9")
 (depends awsm-async awsm-cloud9))

(package
 (name awsm-cloudcontrol)
 (synopsis "AWS API for Cloudcontrol")
 (depends awsm))

(package
 (name awsm-cloudcontrol-async)
 (synopsis "AWS Async API for Cloudcontrol")
 (depends awsm-async awsm-cloudcontrol))

(package
 (name awsm-clouddirectory)
 (synopsis "AWS API for Clouddirectory")
 (depends awsm))

(package
 (name awsm-clouddirectory-async)
 (synopsis "AWS Async API for Clouddirectory")
 (depends awsm-async awsm-clouddirectory))

(package
 (name awsm-cloudformation)
 (synopsis "AWS API for Cloudformation")
 (depends awsm))

(package
 (name awsm-cloudformation-async)
 (synopsis "AWS Async API for Cloudformation")
 (depends awsm-async awsm-cloudformation))

(package
 (name awsm-cloudfront)
 (synopsis "AWS API for Cloudfront")
 (depends awsm))

(package
 (name awsm-cloudfront-async)
 (synopsis "AWS Async API for Cloudfront")
 (depends awsm-async awsm-cloudfront))

(package
 (name awsm-cloudhsmv2)
 (synopsis "AWS API for Cloudhsmv2")
 (depends awsm))

(package
 (name awsm-cloudhsmv2-async)
 (synopsis "AWS Async API for Cloudhsmv2")
 (depends awsm-async awsm-cloudhsmv2))

(package
 (name awsm-cloudsearch)
 (synopsis "AWS API for Cloudsearch")
 (depends awsm))

(package
 (name awsm-cloudsearch-async)
 (synopsis "AWS Async API for Cloudsearch")
 (depends awsm-async awsm-cloudsearch))

(package
 (name awsm-cloudsearchdomain)
 (synopsis "AWS API for Cloudsearchdomain")
 (depends awsm))

(package
 (name awsm-cloudsearchdomain-async)
 (synopsis "AWS Async API for Cloudsearchdomain")
 (depends awsm-async awsm-cloudsearchdomain))

(package
 (name awsm-cloudtrail)
 (synopsis "AWS API for Cloudtrail")
 (depends awsm))

(package
 (name awsm-cloudtrail-async)
 (synopsis "AWS Async API for Cloudtrail")
 (depends awsm-async awsm-cloudtrail))

(package
 (name awsm-cloudwatch)
 (synopsis "AWS API for Cloudwatch")
 (depends awsm))

(package
 (name awsm-cloudwatch-async)
 (synopsis "AWS Async API for Cloudwatch")
 (depends awsm-async awsm-cloudwatch))

(package
 (name awsm-codeartifact)
 (synopsis "AWS API for Codeartifact")
 (depends awsm))

(package
 (name awsm-codeartifact-async)
 (synopsis "AWS Async API for Codeartifact")
 (depends awsm-async awsm-codeartifact))

(package
 (name awsm-codebuild)
 (synopsis "AWS API for Codebuild")
 (depends awsm))

(package
 (name awsm-codebuild-async)
 (synopsis "AWS Async API for Codebuild")
 (depends awsm-async awsm-codebuild))

(package
 (name awsm-codecommit)
 (synopsis "AWS API for Codecommit")
 (depends awsm))

(package
 (name awsm-codecommit-async)
 (synopsis "AWS Async API for Codecommit")
 (depends awsm-async awsm-codecommit))

(package
 (name awsm-codedeploy)
 (synopsis "AWS API for Codedeploy")
 (depends awsm))

(package
 (name awsm-codedeploy-async)
 (synopsis "AWS Async API for Codedeploy")
 (depends awsm-async awsm-codedeploy))

(package
 (name awsm-codeguru-reviewer)
 (synopsis "AWS API for Codeguru-reviewer")
 (depends awsm))

(package
 (name awsm-codeguru-reviewer-async)
 (synopsis "AWS Async API for Codeguru-reviewer")
 (depends awsm-async awsm-codeguru-reviewer))

(package
 (name awsm-codepipeline)
 (synopsis "AWS API for Codepipeline")
 (depends awsm))

(package
 (name awsm-codepipeline-async)
 (synopsis "AWS Async API for Codepipeline")
 (depends awsm-async awsm-codepipeline))

(package
 (name awsm-codestar)
 (synopsis "AWS API for Codestar")
 (depends awsm))

(package
 (name awsm-codestar-async)
 (synopsis "AWS Async API for Codestar")
 (depends awsm-async awsm-codestar))

(package
 (name awsm-codestar-connections)
 (synopsis "AWS API for Codestar-connections")
 (depends awsm))

(package
 (name awsm-codestar-connections-async)
 (synopsis "AWS Async API for Codestar-connections")
 (depends awsm-async awsm-codestar-connections))

(package
 (name awsm-codestar-notifications)
 (synopsis "AWS API for Codestar-notifications")
 (depends awsm))

(package
 (name awsm-codestar-notifications-async)
 (synopsis "AWS Async API for Codestar-notifications")
 (depends awsm-async awsm-codestar-notifications))

(package
 (name awsm-cognito-identity)
 (synopsis "AWS API for Cognito-identity")
 (depends awsm))

(package
 (name awsm-cognito-identity-async)
 (synopsis "AWS Async API for Cognito-identity")
 (depends awsm-async awsm-cognito-identity))

(package
 (name awsm-cognito-idp)
 (synopsis "AWS API for Cognito-idp")
 (depends awsm))

(package
 (name awsm-cognito-idp-async)
 (synopsis "AWS Async API for Cognito-idp")
 (depends awsm-async awsm-cognito-idp))

(package
 (name awsm-cognito-sync)
 (synopsis "AWS API for Cognito-sync")
 (depends awsm))

(package
 (name awsm-cognito-sync-async)
 (synopsis "AWS Async API for Cognito-sync")
 (depends awsm-async awsm-cognito-sync))

(package
 (name awsm-comprehend)
 (synopsis "AWS API for Comprehend")
 (depends awsm))

(package
 (name awsm-comprehend-async)
 (synopsis "AWS Async API for Comprehend")
 (depends awsm-async awsm-comprehend))

(package
 (name awsm-comprehendmedical)
 (synopsis "AWS API for Comprehendmedical")
 (depends awsm))

(package
 (name awsm-comprehendmedical-async)
 (synopsis "AWS Async API for Comprehendmedical")
 (depends awsm-async awsm-comprehendmedical))

(package
 (name awsm-compute-optimizer)
 (synopsis "AWS API for Compute-optimizer")
 (depends awsm))

(package
 (name awsm-compute-optimizer-async)
 (synopsis "AWS Async API for Compute-optimizer")
 (depends awsm-async awsm-compute-optimizer))

(package
 (name awsm-config)
 (synopsis "AWS API for Config")
 (depends awsm))

(package
 (name awsm-config-async)
 (synopsis "AWS Async API for Config")
 (depends awsm-async awsm-config))

(package
 (name awsm-connect)
 (synopsis "AWS API for Connect")
 (depends awsm))

(package
 (name awsm-connect-async)
 (synopsis "AWS Async API for Connect")
 (depends awsm-async awsm-connect))

(package
 (name awsm-connect-contact-lens)
 (synopsis "AWS API for Connect-contact-lens")
 (depends awsm))

(package
 (name awsm-connect-contact-lens-async)
 (synopsis "AWS Async API for Connect-contact-lens")
 (depends awsm-async awsm-connect-contact-lens))

(package
 (name awsm-connectparticipant)
 (synopsis "AWS API for Connectparticipant")
 (depends awsm))

(package
 (name awsm-connectparticipant-async)
 (synopsis "AWS Async API for Connectparticipant")
 (depends awsm-async awsm-connectparticipant))

(package
 (name awsm-cur)
 (synopsis "AWS API for Cur")
 (depends awsm))

(package
 (name awsm-cur-async)
 (synopsis "AWS Async API for Cur")
 (depends awsm-async awsm-cur))

(package
 (name awsm-customer-profiles)
 (synopsis "AWS API for Customer-profiles")
 (depends awsm))

(package
 (name awsm-customer-profiles-async)
 (synopsis "AWS Async API for Customer-profiles")
 (depends awsm-async awsm-customer-profiles))

(package
 (name awsm-databrew)
 (synopsis "AWS API for Databrew")
 (depends awsm))

(package
 (name awsm-databrew-async)
 (synopsis "AWS Async API for Databrew")
 (depends awsm-async awsm-databrew))

(package
 (name awsm-datapipeline)
 (synopsis "AWS API for Datapipeline")
 (depends awsm))

(package
 (name awsm-datapipeline-async)
 (synopsis "AWS Async API for Datapipeline")
 (depends awsm-async awsm-datapipeline))

(package
 (name awsm-datasync)
 (synopsis "AWS API for Datasync")
 (depends awsm))

(package
 (name awsm-datasync-async)
 (synopsis "AWS Async API for Datasync")
 (depends awsm-async awsm-datasync))

(package
 (name awsm-dax)
 (synopsis "AWS API for Dax")
 (depends awsm))

(package
 (name awsm-dax-async)
 (synopsis "AWS Async API for Dax")
 (depends awsm-async awsm-dax))

(package
 (name awsm-detective)
 (synopsis "AWS API for Detective")
 (depends awsm))

(package
 (name awsm-detective-async)
 (synopsis "AWS Async API for Detective")
 (depends awsm-async awsm-detective))

(package
 (name awsm-devicefarm)
 (synopsis "AWS API for Devicefarm")
 (depends awsm))

(package
 (name awsm-devicefarm-async)
 (synopsis "AWS Async API for Devicefarm")
 (depends awsm-async awsm-devicefarm))

(package
 (name awsm-devops-guru)
 (synopsis "AWS API for Devops-guru")
 (depends awsm))

(package
 (name awsm-devops-guru-async)
 (synopsis "AWS Async API for Devops-guru")
 (depends awsm-async awsm-devops-guru))

(package
 (name awsm-directconnect)
 (synopsis "AWS API for Directconnect")
 (depends awsm))

(package
 (name awsm-directconnect-async)
 (synopsis "AWS Async API for Directconnect")
 (depends awsm-async awsm-directconnect))

(package
 (name awsm-discovery)
 (synopsis "AWS API for Discovery")
 (depends awsm))

(package
 (name awsm-discovery-async)
 (synopsis "AWS Async API for Discovery")
 (depends awsm-async awsm-discovery))

(package
 (name awsm-dlm)
 (synopsis "AWS API for Dlm")
 (depends awsm))

(package
 (name awsm-dlm-async)
 (synopsis "AWS Async API for Dlm")
 (depends awsm-async awsm-dlm))

(package
 (name awsm-dms)
 (synopsis "AWS API for Dms")
 (depends awsm))

(package
 (name awsm-dms-async)
 (synopsis "AWS Async API for Dms")
 (depends awsm-async awsm-dms))

(package
 (name awsm-docdb)
 (synopsis "AWS API for Docdb")
 (depends awsm))

(package
 (name awsm-docdb-async)
 (synopsis "AWS Async API for Docdb")
 (depends awsm-async awsm-docdb))

(package
 (name awsm-drs)
 (synopsis "AWS API for Drs")
 (depends awsm))

(package
 (name awsm-drs-async)
 (synopsis "AWS Async API for Drs")
 (depends awsm-async awsm-drs))

(package
 (name awsm-ds)
 (synopsis "AWS API for Ds")
 (depends awsm))

(package
 (name awsm-ds-async)
 (synopsis "AWS Async API for Ds")
 (depends awsm-async awsm-ds))

(package
 (name awsm-dynamodb)
 (synopsis "AWS API for Dynamodb")
 (depends awsm))

(package
 (name awsm-dynamodb-async)
 (synopsis "AWS Async API for Dynamodb")
 (depends awsm-async awsm-dynamodb))

(package
 (name awsm-dynamodbstreams)
 (synopsis "AWS API for Dynamodbstreams")
 (depends awsm))

(package
 (name awsm-dynamodbstreams-async)
 (synopsis "AWS Async API for Dynamodbstreams")
 (depends awsm-async awsm-dynamodbstreams))

(package
 (name awsm-ebs)
 (synopsis "AWS API for Ebs")
 (depends awsm))

(package
 (name awsm-ebs-async)
 (synopsis "AWS Async API for Ebs")
 (depends awsm-async awsm-ebs))

(package
 (name awsm-ec2)
 (synopsis "AWS API for Ec2")
 (depends awsm))

(package
 (name awsm-ec2-async)
 (synopsis "AWS Async API for Ec2")
 (depends awsm-async awsm-ec2))

(package
 (name awsm-ec2-instance-connect)
 (synopsis "AWS API for Ec2-instance-connect")
 (depends awsm))

(package
 (name awsm-ec2-instance-connect-async)
 (synopsis "AWS Async API for Ec2-instance-connect")
 (depends awsm-async awsm-ec2-instance-connect))

(package
 (name awsm-ecr)
 (synopsis "AWS API for Ecr")
 (depends awsm))

(package
 (name awsm-ecr-async)
 (synopsis "AWS Async API for Ecr")
 (depends awsm-async awsm-ecr))

(package
 (name awsm-ecr-public)
 (synopsis "AWS API for Ecr-public")
 (depends awsm))

(package
 (name awsm-ecr-public-async)
 (synopsis "AWS Async API for Ecr-public")
 (depends awsm-async awsm-ecr-public))

(package
 (name awsm-ecs)
 (synopsis "AWS API for Ecs")
 (depends awsm))

(package
 (name awsm-ecs-async)
 (synopsis "AWS Async API for Ecs")
 (depends awsm-async awsm-ecs))

(package
 (name awsm-efs)
 (synopsis "AWS API for Efs")
 (depends awsm))

(package
 (name awsm-efs-async)
 (synopsis "AWS Async API for Efs")
 (depends awsm-async awsm-efs))

(package
 (name awsm-eks)
 (synopsis "AWS API for Eks")
 (depends awsm))

(package
 (name awsm-eks-async)
 (synopsis "AWS Async API for Eks")
 (depends awsm-async awsm-eks))

(package
 (name awsm-elastic-inference)
 (synopsis "AWS API for Elastic-inference")
 (depends awsm))

(package
 (name awsm-elastic-inference-async)
 (synopsis "AWS Async API for Elastic-inference")
 (depends awsm-async awsm-elastic-inference))

(package
 (name awsm-elasticache)
 (synopsis "AWS API for Elasticache")
 (depends awsm))

(package
 (name awsm-elasticache-async)
 (synopsis "AWS Async API for Elasticache")
 (depends awsm-async awsm-elasticache))

(package
 (name awsm-elasticbeanstalk)
 (synopsis "AWS API for Elasticbeanstalk")
 (depends awsm))

(package
 (name awsm-elasticbeanstalk-async)
 (synopsis "AWS Async API for Elasticbeanstalk")
 (depends awsm-async awsm-elasticbeanstalk))

(package
 (name awsm-elastictranscoder)
 (synopsis "AWS API for Elastictranscoder")
 (depends awsm))

(package
 (name awsm-elastictranscoder-async)
 (synopsis "AWS Async API for Elastictranscoder")
 (depends awsm-async awsm-elastictranscoder))

(package
 (name awsm-elb)
 (synopsis "AWS API for Elb")
 (depends awsm))

(package
 (name awsm-elb-async)
 (synopsis "AWS Async API for Elb")
 (depends awsm-async awsm-elb))

(package
 (name awsm-elbv2)
 (synopsis "AWS API for Elbv2")
 (depends awsm))

(package
 (name awsm-elbv2-async)
 (synopsis "AWS Async API for Elbv2")
 (depends awsm-async awsm-elbv2))

(package
 (name awsm-emr)
 (synopsis "AWS API for Emr")
 (depends awsm))

(package
 (name awsm-emr-async)
 (synopsis "AWS Async API for Emr")
 (depends awsm-async awsm-emr))

(package
 (name awsm-emr-containers)
 (synopsis "AWS API for Emr-containers")
 (depends awsm))

(package
 (name awsm-emr-containers-async)
 (synopsis "AWS Async API for Emr-containers")
 (depends awsm-async awsm-emr-containers))

(package
 (name awsm-es)
 (synopsis "AWS API for Es")
 (depends awsm))

(package
 (name awsm-es-async)
 (synopsis "AWS Async API for Es")
 (depends awsm-async awsm-es))

(package
 (name awsm-events)
 (synopsis "AWS API for Events")
 (depends awsm))

(package
 (name awsm-events-async)
 (synopsis "AWS Async API for Events")
 (depends awsm-async awsm-events))

(package
 (name awsm-evidently)
 (synopsis "AWS API for Evidently")
 (depends awsm))

(package
 (name awsm-evidently-async)
 (synopsis "AWS Async API for Evidently")
 (depends awsm-async awsm-evidently))

(package
 (name awsm-finspace)
 (synopsis "AWS API for Finspace")
 (depends awsm))

(package
 (name awsm-finspace-async)
 (synopsis "AWS Async API for Finspace")
 (depends awsm-async awsm-finspace))

(package
 (name awsm-finspace-data)
 (synopsis "AWS API for Finspace-data")
 (depends awsm))

(package
 (name awsm-finspace-data-async)
 (synopsis "AWS Async API for Finspace-data")
 (depends awsm-async awsm-finspace-data))

(package
 (name awsm-firehose)
 (synopsis "AWS API for Firehose")
 (depends awsm))

(package
 (name awsm-firehose-async)
 (synopsis "AWS Async API for Firehose")
 (depends awsm-async awsm-firehose))

(package
 (name awsm-fis)
 (synopsis "AWS API for Fis")
 (depends awsm))

(package
 (name awsm-fis-async)
 (synopsis "AWS Async API for Fis")
 (depends awsm-async awsm-fis))

(package
 (name awsm-fms)
 (synopsis "AWS API for Fms")
 (depends awsm))

(package
 (name awsm-fms-async)
 (synopsis "AWS Async API for Fms")
 (depends awsm-async awsm-fms))

(package
 (name awsm-forecast)
 (synopsis "AWS API for Forecast")
 (depends awsm))

(package
 (name awsm-forecast-async)
 (synopsis "AWS Async API for Forecast")
 (depends awsm-async awsm-forecast))

(package
 (name awsm-forecastquery)
 (synopsis "AWS API for Forecastquery")
 (depends awsm))

(package
 (name awsm-forecastquery-async)
 (synopsis "AWS Async API for Forecastquery")
 (depends awsm-async awsm-forecastquery))

(package
 (name awsm-frauddetector)
 (synopsis "AWS API for Frauddetector")
 (depends awsm))

(package
 (name awsm-frauddetector-async)
 (synopsis "AWS Async API for Frauddetector")
 (depends awsm-async awsm-frauddetector))

(package
 (name awsm-fsx)
 (synopsis "AWS API for Fsx")
 (depends awsm))

(package
 (name awsm-fsx-async)
 (synopsis "AWS Async API for Fsx")
 (depends awsm-async awsm-fsx))

(package
 (name awsm-gamelift)
 (synopsis "AWS API for Gamelift")
 (depends awsm))

(package
 (name awsm-gamelift-async)
 (synopsis "AWS Async API for Gamelift")
 (depends awsm-async awsm-gamelift))

(package
 (name awsm-gamesparks)
 (synopsis "AWS API for Gamesparks")
 (depends awsm))

(package
 (name awsm-gamesparks-async)
 (synopsis "AWS Async API for Gamesparks")
 (depends awsm-async awsm-gamesparks))

(package
 (name awsm-glacier)
 (synopsis "AWS API for Glacier")
 (depends awsm))

(package
 (name awsm-glacier-async)
 (synopsis "AWS Async API for Glacier")
 (depends awsm-async awsm-glacier))

(package
 (name awsm-globalaccelerator)
 (synopsis "AWS API for Globalaccelerator")
 (depends awsm))

(package
 (name awsm-globalaccelerator-async)
 (synopsis "AWS Async API for Globalaccelerator")
 (depends awsm-async awsm-globalaccelerator))

(package
 (name awsm-glue)
 (synopsis "AWS API for Glue")
 (depends awsm))

(package
 (name awsm-glue-async)
 (synopsis "AWS Async API for Glue")
 (depends awsm-async awsm-glue))

(package
 (name awsm-grafana)
 (synopsis "AWS API for Grafana")
 (depends awsm))

(package
 (name awsm-grafana-async)
 (synopsis "AWS Async API for Grafana")
 (depends awsm-async awsm-grafana))

(package
 (name awsm-greengrass)
 (synopsis "AWS API for Greengrass")
 (depends awsm))

(package
 (name awsm-greengrass-async)
 (synopsis "AWS Async API for Greengrass")
 (depends awsm-async awsm-greengrass))

(package
 (name awsm-greengrassv2)
 (synopsis "AWS API for Greengrassv2")
 (depends awsm))

(package
 (name awsm-greengrassv2-async)
 (synopsis "AWS Async API for Greengrassv2")
 (depends awsm-async awsm-greengrassv2))

(package
 (name awsm-groundstation)
 (synopsis "AWS API for Groundstation")
 (depends awsm))

(package
 (name awsm-groundstation-async)
 (synopsis "AWS Async API for Groundstation")
 (depends awsm-async awsm-groundstation))

(package
 (name awsm-guardduty)
 (synopsis "AWS API for Guardduty")
 (depends awsm))

(package
 (name awsm-guardduty-async)
 (synopsis "AWS Async API for Guardduty")
 (depends awsm-async awsm-guardduty))

(package
 (name awsm-healthlake)
 (synopsis "AWS API for Healthlake")
 (depends awsm))

(package
 (name awsm-healthlake-async)
 (synopsis "AWS Async API for Healthlake")
 (depends awsm-async awsm-healthlake))

(package
 (name awsm-honeycode)
 (synopsis "AWS API for Honeycode")
 (depends awsm))

(package
 (name awsm-honeycode-async)
 (synopsis "AWS Async API for Honeycode")
 (depends awsm-async awsm-honeycode))

(package
 (name awsm-iam)
 (synopsis "AWS API for Iam")
 (depends awsm))

(package
 (name awsm-iam-async)
 (synopsis "AWS Async API for Iam")
 (depends awsm-async awsm-iam))

(package
 (name awsm-identitystore)
 (synopsis "AWS API for Identitystore")
 (depends awsm))

(package
 (name awsm-identitystore-async)
 (synopsis "AWS Async API for Identitystore")
 (depends awsm-async awsm-identitystore))

(package
 (name awsm-imagebuilder)
 (synopsis "AWS API for Imagebuilder")
 (depends awsm))

(package
 (name awsm-imagebuilder-async)
 (synopsis "AWS Async API for Imagebuilder")
 (depends awsm-async awsm-imagebuilder))

(package
 (name awsm-importexport)
 (synopsis "AWS API for Importexport")
 (depends awsm))

(package
 (name awsm-importexport-async)
 (synopsis "AWS Async API for Importexport")
 (depends awsm-async awsm-importexport))

(package
 (name awsm-inspector)
 (synopsis "AWS API for Inspector")
 (depends awsm))

(package
 (name awsm-inspector-async)
 (synopsis "AWS Async API for Inspector")
 (depends awsm-async awsm-inspector))

(package
 (name awsm-inspector2)
 (synopsis "AWS API for Inspector2")
 (depends awsm))

(package
 (name awsm-inspector2-async)
 (synopsis "AWS Async API for Inspector2")
 (depends awsm-async awsm-inspector2))

(package
 (name awsm-iot)
 (synopsis "AWS API for Iot")
 (depends awsm))

(package
 (name awsm-iot-async)
 (synopsis "AWS Async API for Iot")
 (depends awsm-async awsm-iot))

(package
 (name awsm-iot-data)
 (synopsis "AWS API for Iot-data")
 (depends awsm))

(package
 (name awsm-iot-data-async)
 (synopsis "AWS Async API for Iot-data")
 (depends awsm-async awsm-iot-data))

(package
 (name awsm-iot-jobs-data)
 (synopsis "AWS API for Iot-jobs-data")
 (depends awsm))

(package
 (name awsm-iot-jobs-data-async)
 (synopsis "AWS Async API for Iot-jobs-data")
 (depends awsm-async awsm-iot-jobs-data))

(package
 (name awsm-iot1click-devices)
 (synopsis "AWS API for Iot1click-devices")
 (depends awsm))

(package
 (name awsm-iot1click-devices-async)
 (synopsis "AWS Async API for Iot1click-devices")
 (depends awsm-async awsm-iot1click-devices))

(package
 (name awsm-iot1click-projects)
 (synopsis "AWS API for Iot1click-projects")
 (depends awsm))

(package
 (name awsm-iot1click-projects-async)
 (synopsis "AWS Async API for Iot1click-projects")
 (depends awsm-async awsm-iot1click-projects))

(package
 (name awsm-iotanalytics)
 (synopsis "AWS API for Iotanalytics")
 (depends awsm))

(package
 (name awsm-iotanalytics-async)
 (synopsis "AWS Async API for Iotanalytics")
 (depends awsm-async awsm-iotanalytics))

(package
 (name awsm-iotdeviceadvisor)
 (synopsis "AWS API for Iotdeviceadvisor")
 (depends awsm))

(package
 (name awsm-iotdeviceadvisor-async)
 (synopsis "AWS Async API for Iotdeviceadvisor")
 (depends awsm-async awsm-iotdeviceadvisor))

(package
 (name awsm-iotevents)
 (synopsis "AWS API for Iotevents")
 (depends awsm))

(package
 (name awsm-iotevents-async)
 (synopsis "AWS Async API for Iotevents")
 (depends awsm-async awsm-iotevents))

(package
 (name awsm-iotevents-data)
 (synopsis "AWS API for Iotevents-data")
 (depends awsm))

(package
 (name awsm-iotevents-data-async)
 (synopsis "AWS Async API for Iotevents-data")
 (depends awsm-async awsm-iotevents-data))

(package
 (name awsm-iotfleethub)
 (synopsis "AWS API for Iotfleethub")
 (depends awsm))

(package
 (name awsm-iotfleethub-async)
 (synopsis "AWS Async API for Iotfleethub")
 (depends awsm-async awsm-iotfleethub))

(package
 (name awsm-iotsecuretunneling)
 (synopsis "AWS API for Iotsecuretunneling")
 (depends awsm))

(package
 (name awsm-iotsecuretunneling-async)
 (synopsis "AWS Async API for Iotsecuretunneling")
 (depends awsm-async awsm-iotsecuretunneling))

(package
 (name awsm-iotsitewise)
 (synopsis "AWS API for Iotsitewise")
 (depends awsm))

(package
 (name awsm-iotsitewise-async)
 (synopsis "AWS Async API for Iotsitewise")
 (depends awsm-async awsm-iotsitewise))

(package
 (name awsm-iotthingsgraph)
 (synopsis "AWS API for Iotthingsgraph")
 (depends awsm))

(package
 (name awsm-iotthingsgraph-async)
 (synopsis "AWS Async API for Iotthingsgraph")
 (depends awsm-async awsm-iotthingsgraph))

(package
 (name awsm-iotwireless)
 (synopsis "AWS API for Iotwireless")
 (depends awsm))

(package
 (name awsm-iotwireless-async)
 (synopsis "AWS Async API for Iotwireless")
 (depends awsm-async awsm-iotwireless))

(package
 (name awsm-ivs)
 (synopsis "AWS API for Ivs")
 (depends awsm))

(package
 (name awsm-ivs-async)
 (synopsis "AWS Async API for Ivs")
 (depends awsm-async awsm-ivs))

(package
 (name awsm-kafka)
 (synopsis "AWS API for Kafka")
 (depends awsm))

(package
 (name awsm-kafka-async)
 (synopsis "AWS Async API for Kafka")
 (depends awsm-async awsm-kafka))

(package
 (name awsm-kafkaconnect)
 (synopsis "AWS API for Kafkaconnect")
 (depends awsm))

(package
 (name awsm-kafkaconnect-async)
 (synopsis "AWS Async API for Kafkaconnect")
 (depends awsm-async awsm-kafkaconnect))

(package
 (name awsm-kendra)
 (synopsis "AWS API for Kendra")
 (depends awsm))

(package
 (name awsm-kendra-async)
 (synopsis "AWS Async API for Kendra")
 (depends awsm-async awsm-kendra))

(package
 (name awsm-keyspaces)
 (synopsis "AWS API for Keyspaces")
 (depends awsm))

(package
 (name awsm-keyspaces-async)
 (synopsis "AWS Async API for Keyspaces")
 (depends awsm-async awsm-keyspaces))

(package
 (name awsm-kinesis)
 (synopsis "AWS API for Kinesis")
 (depends awsm))

(package
 (name awsm-kinesis-async)
 (synopsis "AWS Async API for Kinesis")
 (depends awsm-async awsm-kinesis))

(package
 (name awsm-kinesis-video-archived-media)
 (synopsis "AWS API for Kinesis-video-archived-media")
 (depends awsm))

(package
 (name awsm-kinesis-video-archived-media-async)
 (synopsis "AWS Async API for Kinesis-video-archived-media")
 (depends awsm-async awsm-kinesis-video-archived-media))

(package
 (name awsm-kinesis-video-media)
 (synopsis "AWS API for Kinesis-video-media")
 (depends awsm))

(package
 (name awsm-kinesis-video-media-async)
 (synopsis "AWS Async API for Kinesis-video-media")
 (depends awsm-async awsm-kinesis-video-media))

(package
 (name awsm-kinesis-video-signaling)
 (synopsis "AWS API for Kinesis-video-signaling")
 (depends awsm))

(package
 (name awsm-kinesis-video-signaling-async)
 (synopsis "AWS Async API for Kinesis-video-signaling")
 (depends awsm-async awsm-kinesis-video-signaling))

(package
 (name awsm-kinesisanalytics)
 (synopsis "AWS API for Kinesisanalytics")
 (depends awsm))

(package
 (name awsm-kinesisanalytics-async)
 (synopsis "AWS Async API for Kinesisanalytics")
 (depends awsm-async awsm-kinesisanalytics))

(package
 (name awsm-kinesisanalyticsv2)
 (synopsis "AWS API for Kinesisanalyticsv2")
 (depends awsm))

(package
 (name awsm-kinesisanalyticsv2-async)
 (synopsis "AWS Async API for Kinesisanalyticsv2")
 (depends awsm-async awsm-kinesisanalyticsv2))

(package
 (name awsm-kinesisvideo)
 (synopsis "AWS API for Kinesisvideo")
 (depends awsm))

(package
 (name awsm-kinesisvideo-async)
 (synopsis "AWS Async API for Kinesisvideo")
 (depends awsm-async awsm-kinesisvideo))

(package
 (name awsm-kms)
 (synopsis "AWS API for Kms")
 (depends awsm))

(package
 (name awsm-kms-async)
 (synopsis "AWS Async API for Kms")
 (depends awsm-async awsm-kms))

(package
 (name awsm-lakeformation)
 (synopsis "AWS API for Lakeformation")
 (depends awsm))

(package
 (name awsm-lakeformation-async)
 (synopsis "AWS Async API for Lakeformation")
 (depends awsm-async awsm-lakeformation))

(package
 (name awsm-lambda)
 (synopsis "AWS API for Lambda")
 (depends awsm))

(package
 (name awsm-lambda-async)
 (synopsis "AWS Async API for Lambda")
 (depends awsm-async awsm-lambda))

(package
 (name awsm-lex-models)
 (synopsis "AWS API for Lex-models")
 (depends awsm))

(package
 (name awsm-lex-models-async)
 (synopsis "AWS Async API for Lex-models")
 (depends awsm-async awsm-lex-models))

(package
 (name awsm-lexv2-models)
 (synopsis "AWS API for Lexv2-models")
 (depends awsm))

(package
 (name awsm-lexv2-models-async)
 (synopsis "AWS Async API for Lexv2-models")
 (depends awsm-async awsm-lexv2-models))

(package
 (name awsm-license-manager)
 (synopsis "AWS API for License-manager")
 (depends awsm))

(package
 (name awsm-license-manager-async)
 (synopsis "AWS Async API for License-manager")
 (depends awsm-async awsm-license-manager))

(package
 (name awsm-lightsail)
 (synopsis "AWS API for Lightsail")
 (depends awsm))

(package
 (name awsm-lightsail-async)
 (synopsis "AWS Async API for Lightsail")
 (depends awsm-async awsm-lightsail))

(package
 (name awsm-location)
 (synopsis "AWS API for Location")
 (depends awsm))

(package
 (name awsm-location-async)
 (synopsis "AWS Async API for Location")
 (depends awsm-async awsm-location))

(package
 (name awsm-logs)
 (synopsis "AWS API for Logs")
 (depends awsm))

(package
 (name awsm-logs-async)
 (synopsis "AWS Async API for Logs")
 (depends awsm-async awsm-logs))

(package
 (name awsm-lookoutequipment)
 (synopsis "AWS API for Lookoutequipment")
 (depends awsm))

(package
 (name awsm-lookoutequipment-async)
 (synopsis "AWS Async API for Lookoutequipment")
 (depends awsm-async awsm-lookoutequipment))

(package
 (name awsm-lookoutmetrics)
 (synopsis "AWS API for Lookoutmetrics")
 (depends awsm))

(package
 (name awsm-lookoutmetrics-async)
 (synopsis "AWS Async API for Lookoutmetrics")
 (depends awsm-async awsm-lookoutmetrics))

(package
 (name awsm-machinelearning)
 (synopsis "AWS API for Machinelearning")
 (depends awsm))

(package
 (name awsm-machinelearning-async)
 (synopsis "AWS Async API for Machinelearning")
 (depends awsm-async awsm-machinelearning))

(package
 (name awsm-macie)
 (synopsis "AWS API for Macie")
 (depends awsm))

(package
 (name awsm-macie-async)
 (synopsis "AWS Async API for Macie")
 (depends awsm-async awsm-macie))

(package
 (name awsm-macie2)
 (synopsis "AWS API for Macie2")
 (depends awsm))

(package
 (name awsm-macie2-async)
 (synopsis "AWS Async API for Macie2")
 (depends awsm-async awsm-macie2))

(package
 (name awsm-managedblockchain)
 (synopsis "AWS API for Managedblockchain")
 (depends awsm))

(package
 (name awsm-managedblockchain-async)
 (synopsis "AWS Async API for Managedblockchain")
 (depends awsm-async awsm-managedblockchain))

(package
 (name awsm-marketplace-catalog)
 (synopsis "AWS API for Marketplace-catalog")
 (depends awsm))

(package
 (name awsm-marketplace-catalog-async)
 (synopsis "AWS Async API for Marketplace-catalog")
 (depends awsm-async awsm-marketplace-catalog))

(package
 (name awsm-marketplace-entitlement)
 (synopsis "AWS API for Marketplace-entitlement")
 (depends awsm))

(package
 (name awsm-marketplace-entitlement-async)
 (synopsis "AWS Async API for Marketplace-entitlement")
 (depends awsm-async awsm-marketplace-entitlement))

(package
 (name awsm-marketplacecommerceanalytics)
 (synopsis "AWS API for Marketplacecommerceanalytics")
 (depends awsm))

(package
 (name awsm-marketplacecommerceanalytics-async)
 (synopsis "AWS Async API for Marketplacecommerceanalytics")
 (depends awsm-async awsm-marketplacecommerceanalytics))

(package
 (name awsm-mediaconvert)
 (synopsis "AWS API for Mediaconvert")
 (depends awsm))

(package
 (name awsm-mediaconvert-async)
 (synopsis "AWS Async API for Mediaconvert")
 (depends awsm-async awsm-mediaconvert))

(package
 (name awsm-mediapackage)
 (synopsis "AWS API for Mediapackage")
 (depends awsm))

(package
 (name awsm-mediapackage-async)
 (synopsis "AWS Async API for Mediapackage")
 (depends awsm-async awsm-mediapackage))

(package
 (name awsm-mediapackage-vod)
 (synopsis "AWS API for Mediapackage-vod")
 (depends awsm))

(package
 (name awsm-mediapackage-vod-async)
 (synopsis "AWS Async API for Mediapackage-vod")
 (depends awsm-async awsm-mediapackage-vod))

(package
 (name awsm-mediastore)
 (synopsis "AWS API for Mediastore")
 (depends awsm))

(package
 (name awsm-mediastore-async)
 (synopsis "AWS Async API for Mediastore")
 (depends awsm-async awsm-mediastore))

(package
 (name awsm-mediastore-data)
 (synopsis "AWS API for Mediastore-data")
 (depends awsm))

(package
 (name awsm-mediastore-data-async)
 (synopsis "AWS Async API for Mediastore-data")
 (depends awsm-async awsm-mediastore-data))

(package
 (name awsm-mediatailor)
 (synopsis "AWS API for Mediatailor")
 (depends awsm))

(package
 (name awsm-mediatailor-async)
 (synopsis "AWS Async API for Mediatailor")
 (depends awsm-async awsm-mediatailor))

(package
 (name awsm-memorydb)
 (synopsis "AWS API for Memorydb")
 (depends awsm))

(package
 (name awsm-memorydb-async)
 (synopsis "AWS Async API for Memorydb")
 (depends awsm-async awsm-memorydb))

(package
 (name awsm-meteringmarketplace)
 (synopsis "AWS API for Meteringmarketplace")
 (depends awsm))

(package
 (name awsm-meteringmarketplace-async)
 (synopsis "AWS Async API for Meteringmarketplace")
 (depends awsm-async awsm-meteringmarketplace))

(package
 (name awsm-mgh)
 (synopsis "AWS API for Mgh")
 (depends awsm))

(package
 (name awsm-mgh-async)
 (synopsis "AWS Async API for Mgh")
 (depends awsm-async awsm-mgh))

(package
 (name awsm-mgn)
 (synopsis "AWS API for Mgn")
 (depends awsm))

(package
 (name awsm-mgn-async)
 (synopsis "AWS Async API for Mgn")
 (depends awsm-async awsm-mgn))

(package
 (name awsm-migration-hub-refactor-spaces)
 (synopsis "AWS API for Migration-hub-refactor-spaces")
 (depends awsm))

(package
 (name awsm-migration-hub-refactor-spaces-async)
 (synopsis "AWS Async API for Migration-hub-refactor-spaces")
 (depends awsm-async awsm-migration-hub-refactor-spaces))

(package
 (name awsm-migrationhub-config)
 (synopsis "AWS API for Migrationhub-config")
 (depends awsm))

(package
 (name awsm-migrationhub-config-async)
 (synopsis "AWS Async API for Migrationhub-config")
 (depends awsm-async awsm-migrationhub-config))

(package
 (name awsm-migrationhubstrategy)
 (synopsis "AWS API for Migrationhubstrategy")
 (depends awsm))

(package
 (name awsm-migrationhubstrategy-async)
 (synopsis "AWS Async API for Migrationhubstrategy")
 (depends awsm-async awsm-migrationhubstrategy))

(package
 (name awsm-mobile)
 (synopsis "AWS API for Mobile")
 (depends awsm))

(package
 (name awsm-mobile-async)
 (synopsis "AWS Async API for Mobile")
 (depends awsm-async awsm-mobile))

(package
 (name awsm-mturk)
 (synopsis "AWS API for Mturk")
 (depends awsm))

(package
 (name awsm-mturk-async)
 (synopsis "AWS Async API for Mturk")
 (depends awsm-async awsm-mturk))

(package
 (name awsm-mwaa)
 (synopsis "AWS API for Mwaa")
 (depends awsm))

(package
 (name awsm-mwaa-async)
 (synopsis "AWS Async API for Mwaa")
 (depends awsm-async awsm-mwaa))

(package
 (name awsm-neptune)
 (synopsis "AWS API for Neptune")
 (depends awsm))

(package
 (name awsm-neptune-async)
 (synopsis "AWS Async API for Neptune")
 (depends awsm-async awsm-neptune))

(package
 (name awsm-network-firewall)
 (synopsis "AWS API for Network-firewall")
 (depends awsm))

(package
 (name awsm-network-firewall-async)
 (synopsis "AWS Async API for Network-firewall")
 (depends awsm-async awsm-network-firewall))

(package
 (name awsm-networkmanager)
 (synopsis "AWS API for Networkmanager")
 (depends awsm))

(package
 (name awsm-networkmanager-async)
 (synopsis "AWS Async API for Networkmanager")
 (depends awsm-async awsm-networkmanager))

(package
 (name awsm-nimble)
 (synopsis "AWS API for Nimble")
 (depends awsm))

(package
 (name awsm-nimble-async)
 (synopsis "AWS Async API for Nimble")
 (depends awsm-async awsm-nimble))

(package
 (name awsm-opensearch)
 (synopsis "AWS API for Opensearch")
 (depends awsm))

(package
 (name awsm-opensearch-async)
 (synopsis "AWS Async API for Opensearch")
 (depends awsm-async awsm-opensearch))

(package
 (name awsm-opsworks)
 (synopsis "AWS API for Opsworks")
 (depends awsm))

(package
 (name awsm-opsworks-async)
 (synopsis "AWS Async API for Opsworks")
 (depends awsm-async awsm-opsworks))

(package
 (name awsm-opsworkscm)
 (synopsis "AWS API for Opsworkscm")
 (depends awsm))

(package
 (name awsm-opsworkscm-async)
 (synopsis "AWS Async API for Opsworkscm")
 (depends awsm-async awsm-opsworkscm))

(package
 (name awsm-organizations)
 (synopsis "AWS API for Organizations")
 (depends awsm))

(package
 (name awsm-organizations-async)
 (synopsis "AWS Async API for Organizations")
 (depends awsm-async awsm-organizations))

(package
 (name awsm-outposts)
 (synopsis "AWS API for Outposts")
 (depends awsm))

(package
 (name awsm-outposts-async)
 (synopsis "AWS Async API for Outposts")
 (depends awsm-async awsm-outposts))

(package
 (name awsm-panorama)
 (synopsis "AWS API for Panorama")
 (depends awsm))

(package
 (name awsm-panorama-async)
 (synopsis "AWS Async API for Panorama")
 (depends awsm-async awsm-panorama))

(package
 (name awsm-personalize)
 (synopsis "AWS API for Personalize")
 (depends awsm))

(package
 (name awsm-personalize-async)
 (synopsis "AWS Async API for Personalize")
 (depends awsm-async awsm-personalize))

(package
 (name awsm-personalize-events)
 (synopsis "AWS API for Personalize-events")
 (depends awsm))

(package
 (name awsm-personalize-events-async)
 (synopsis "AWS Async API for Personalize-events")
 (depends awsm-async awsm-personalize-events))

(package
 (name awsm-personalize-runtime)
 (synopsis "AWS API for Personalize-runtime")
 (depends awsm))

(package
 (name awsm-personalize-runtime-async)
 (synopsis "AWS Async API for Personalize-runtime")
 (depends awsm-async awsm-personalize-runtime))

(package
 (name awsm-pi)
 (synopsis "AWS API for Pi")
 (depends awsm))

(package
 (name awsm-pi-async)
 (synopsis "AWS Async API for Pi")
 (depends awsm-async awsm-pi))

(package
 (name awsm-pinpoint-email)
 (synopsis "AWS API for Pinpoint-email")
 (depends awsm))

(package
 (name awsm-pinpoint-email-async)
 (synopsis "AWS Async API for Pinpoint-email")
 (depends awsm-async awsm-pinpoint-email))

(package
 (name awsm-pinpoint-sms-voice)
 (synopsis "AWS API for Pinpoint-sms-voice")
 (depends awsm))

(package
 (name awsm-pinpoint-sms-voice-async)
 (synopsis "AWS Async API for Pinpoint-sms-voice")
 (depends awsm-async awsm-pinpoint-sms-voice))

(package
 (name awsm-pinpoint-sms-voice-v2)
 (synopsis "AWS API for Pinpoint-sms-voice-v2")
 (depends awsm))

(package
 (name awsm-pinpoint-sms-voice-v2-async)
 (synopsis "AWS Async API for Pinpoint-sms-voice-v2")
 (depends awsm-async awsm-pinpoint-sms-voice-v2))

(package
 (name awsm-polly)
 (synopsis "AWS API for Polly")
 (depends awsm))

(package
 (name awsm-polly-async)
 (synopsis "AWS Async API for Polly")
 (depends awsm-async awsm-polly))

(package
 (name awsm-pricing)
 (synopsis "AWS API for Pricing")
 (depends awsm))

(package
 (name awsm-pricing-async)
 (synopsis "AWS Async API for Pricing")
 (depends awsm-async awsm-pricing))

(package
 (name awsm-proton)
 (synopsis "AWS API for Proton")
 (depends awsm))

(package
 (name awsm-proton-async)
 (synopsis "AWS Async API for Proton")
 (depends awsm-async awsm-proton))

(package
 (name awsm-qldb)
 (synopsis "AWS API for Qldb")
 (depends awsm))

(package
 (name awsm-qldb-async)
 (synopsis "AWS Async API for Qldb")
 (depends awsm-async awsm-qldb))

(package
 (name awsm-qldb-session)
 (synopsis "AWS API for Qldb-session")
 (depends awsm))

(package
 (name awsm-qldb-session-async)
 (synopsis "AWS Async API for Qldb-session")
 (depends awsm-async awsm-qldb-session))

(package
 (name awsm-quicksight)
 (synopsis "AWS API for Quicksight")
 (depends awsm))

(package
 (name awsm-quicksight-async)
 (synopsis "AWS Async API for Quicksight")
 (depends awsm-async awsm-quicksight))

(package
 (name awsm-ram)
 (synopsis "AWS API for Ram")
 (depends awsm))

(package
 (name awsm-ram-async)
 (synopsis "AWS Async API for Ram")
 (depends awsm-async awsm-ram))

(package
 (name awsm-rbin)
 (synopsis "AWS API for Rbin")
 (depends awsm))

(package
 (name awsm-rbin-async)
 (synopsis "AWS Async API for Rbin")
 (depends awsm-async awsm-rbin))

(package
 (name awsm-rds)
 (synopsis "AWS API for Rds")
 (depends awsm))

(package
 (name awsm-rds-async)
 (synopsis "AWS Async API for Rds")
 (depends awsm-async awsm-rds))

(package
 (name awsm-rds-data)
 (synopsis "AWS API for Rds-data")
 (depends awsm))

(package
 (name awsm-rds-data-async)
 (synopsis "AWS Async API for Rds-data")
 (depends awsm-async awsm-rds-data))

(package
 (name awsm-redshift)
 (synopsis "AWS API for Redshift")
 (depends awsm))

(package
 (name awsm-redshift-async)
 (synopsis "AWS Async API for Redshift")
 (depends awsm-async awsm-redshift))

(package
 (name awsm-redshift-data)
 (synopsis "AWS API for Redshift-data")
 (depends awsm))

(package
 (name awsm-redshift-data-async)
 (synopsis "AWS Async API for Redshift-data")
 (depends awsm-async awsm-redshift-data))

(package
 (name awsm-rekognition)
 (synopsis "AWS API for Rekognition")
 (depends awsm))

(package
 (name awsm-rekognition-async)
 (synopsis "AWS Async API for Rekognition")
 (depends awsm-async awsm-rekognition))

(package
 (name awsm-resiliencehub)
 (synopsis "AWS API for Resiliencehub")
 (depends awsm))

(package
 (name awsm-resiliencehub-async)
 (synopsis "AWS Async API for Resiliencehub")
 (depends awsm-async awsm-resiliencehub))

(package
 (name awsm-resource-groups)
 (synopsis "AWS API for Resource-groups")
 (depends awsm))

(package
 (name awsm-resource-groups-async)
 (synopsis "AWS Async API for Resource-groups")
 (depends awsm-async awsm-resource-groups))

(package
 (name awsm-resourcegroupstaggingapi)
 (synopsis "AWS API for Resourcegroupstaggingapi")
 (depends awsm))

(package
 (name awsm-resourcegroupstaggingapi-async)
 (synopsis "AWS Async API for Resourcegroupstaggingapi")
 (depends awsm-async awsm-resourcegroupstaggingapi))

(package
 (name awsm-robomaker)
 (synopsis "AWS API for Robomaker")
 (depends awsm))

(package
 (name awsm-robomaker-async)
 (synopsis "AWS Async API for Robomaker")
 (depends awsm-async awsm-robomaker))

(package
 (name awsm-route53)
 (synopsis "AWS API for Route53")
 (depends awsm))

(package
 (name awsm-route53-async)
 (synopsis "AWS Async API for Route53")
 (depends awsm-async awsm-route53))

(package
 (name awsm-route53-recovery-cluster)
 (synopsis "AWS API for Route53-recovery-cluster")
 (depends awsm))

(package
 (name awsm-route53-recovery-cluster-async)
 (synopsis "AWS Async API for Route53-recovery-cluster")
 (depends awsm-async awsm-route53-recovery-cluster))

(package
 (name awsm-route53-recovery-control-config)
 (synopsis "AWS API for Route53-recovery-control-config")
 (depends awsm))

(package
 (name awsm-route53-recovery-control-config-async)
 (synopsis "AWS Async API for Route53-recovery-control-config")
 (depends awsm-async awsm-route53-recovery-control-config))

(package
 (name awsm-route53-recovery-readiness)
 (synopsis "AWS API for Route53-recovery-readiness")
 (depends awsm))

(package
 (name awsm-route53-recovery-readiness-async)
 (synopsis "AWS Async API for Route53-recovery-readiness")
 (depends awsm-async awsm-route53-recovery-readiness))

(package
 (name awsm-route53domains)
 (synopsis "AWS API for Route53domains")
 (depends awsm))

(package
 (name awsm-route53domains-async)
 (synopsis "AWS Async API for Route53domains")
 (depends awsm-async awsm-route53domains))

(package
 (name awsm-route53resolver)
 (synopsis "AWS API for Route53resolver")
 (depends awsm))

(package
 (name awsm-route53resolver-async)
 (synopsis "AWS Async API for Route53resolver")
 (depends awsm-async awsm-route53resolver))

(package
 (name awsm-rum)
 (synopsis "AWS API for Rum")
 (depends awsm))

(package
 (name awsm-rum-async)
 (synopsis "AWS Async API for Rum")
 (depends awsm-async awsm-rum))

(package
 (name awsm-s3)
 (synopsis "AWS API for S3")
 (depends awsm))

(package
 (name awsm-s3-async)
 (synopsis "AWS Async API for S3")
 (depends awsm-async awsm-s3))

(package
 (name awsm-s3outposts)
 (synopsis "AWS API for S3outposts")
 (depends awsm))

(package
 (name awsm-s3outposts-async)
 (synopsis "AWS Async API for S3outposts")
 (depends awsm-async awsm-s3outposts))

(package
 (name awsm-sagemaker)
 (synopsis "AWS API for Sagemaker")
 (depends awsm))

(package
 (name awsm-sagemaker-async)
 (synopsis "AWS Async API for Sagemaker")
 (depends awsm-async awsm-sagemaker))

(package
 (name awsm-sagemaker-a2i-runtime)
 (synopsis "AWS API for Sagemaker-a2i-runtime")
 (depends awsm))

(package
 (name awsm-sagemaker-a2i-runtime-async)
 (synopsis "AWS Async API for Sagemaker-a2i-runtime")
 (depends awsm-async awsm-sagemaker-a2i-runtime))

(package
 (name awsm-sagemaker-edge)
 (synopsis "AWS API for Sagemaker-edge")
 (depends awsm))

(package
 (name awsm-sagemaker-edge-async)
 (synopsis "AWS Async API for Sagemaker-edge")
 (depends awsm-async awsm-sagemaker-edge))

(package
 (name awsm-sagemaker-featurestore-runtime)
 (synopsis "AWS API for Sagemaker-featurestore-runtime")
 (depends awsm))

(package
 (name awsm-sagemaker-featurestore-runtime-async)
 (synopsis "AWS Async API for Sagemaker-featurestore-runtime")
 (depends awsm-async awsm-sagemaker-featurestore-runtime))

(package
 (name awsm-savingsplans)
 (synopsis "AWS API for Savingsplans")
 (depends awsm))

(package
 (name awsm-savingsplans-async)
 (synopsis "AWS Async API for Savingsplans")
 (depends awsm-async awsm-savingsplans))

(package
 (name awsm-schemas)
 (synopsis "AWS API for Schemas")
 (depends awsm))

(package
 (name awsm-schemas-async)
 (synopsis "AWS Async API for Schemas")
 (depends awsm-async awsm-schemas))

(package
 (name awsm-sdb)
 (synopsis "AWS API for Sdb")
 (depends awsm))

(package
 (name awsm-sdb-async)
 (synopsis "AWS Async API for Sdb")
 (depends awsm-async awsm-sdb))

(package
 (name awsm-secretsmanager)
 (synopsis "AWS API for Secretsmanager")
 (depends awsm))

(package
 (name awsm-secretsmanager-async)
 (synopsis "AWS Async API for Secretsmanager")
 (depends awsm-async awsm-secretsmanager))

(package
 (name awsm-securityhub)
 (synopsis "AWS API for Securityhub")
 (depends awsm))

(package
 (name awsm-securityhub-async)
 (synopsis "AWS Async API for Securityhub")
 (depends awsm-async awsm-securityhub))

(package
 (name awsm-serverlessrepo)
 (synopsis "AWS API for Serverlessrepo")
 (depends awsm))

(package
 (name awsm-serverlessrepo-async)
 (synopsis "AWS Async API for Serverlessrepo")
 (depends awsm-async awsm-serverlessrepo))

(package
 (name awsm-service-quotas)
 (synopsis "AWS API for Service-quotas")
 (depends awsm))

(package
 (name awsm-service-quotas-async)
 (synopsis "AWS Async API for Service-quotas")
 (depends awsm-async awsm-service-quotas))

(package
 (name awsm-servicecatalog)
 (synopsis "AWS API for Servicecatalog")
 (depends awsm))

(package
 (name awsm-servicecatalog-async)
 (synopsis "AWS Async API for Servicecatalog")
 (depends awsm-async awsm-servicecatalog))

(package
 (name awsm-servicecatalog-appregistry)
 (synopsis "AWS API for Servicecatalog-appregistry")
 (depends awsm))

(package
 (name awsm-servicecatalog-appregistry-async)
 (synopsis "AWS Async API for Servicecatalog-appregistry")
 (depends awsm-async awsm-servicecatalog-appregistry))

(package
 (name awsm-servicediscovery)
 (synopsis "AWS API for Servicediscovery")
 (depends awsm))

(package
 (name awsm-servicediscovery-async)
 (synopsis "AWS Async API for Servicediscovery")
 (depends awsm-async awsm-servicediscovery))

(package
 (name awsm-ses)
 (synopsis "AWS API for Ses")
 (depends awsm))

(package
 (name awsm-ses-async)
 (synopsis "AWS Async API for Ses")
 (depends awsm-async awsm-ses))

(package
 (name awsm-sesv2)
 (synopsis "AWS API for Sesv2")
 (depends awsm))

(package
 (name awsm-sesv2-async)
 (synopsis "AWS Async API for Sesv2")
 (depends awsm-async awsm-sesv2))

(package
 (name awsm-shield)
 (synopsis "AWS API for Shield")
 (depends awsm))

(package
 (name awsm-shield-async)
 (synopsis "AWS Async API for Shield")
 (depends awsm-async awsm-shield))

(package
 (name awsm-signer)
 (synopsis "AWS API for Signer")
 (depends awsm))

(package
 (name awsm-signer-async)
 (synopsis "AWS Async API for Signer")
 (depends awsm-async awsm-signer))

(package
 (name awsm-sms)
 (synopsis "AWS API for Sms")
 (depends awsm))

(package
 (name awsm-sms-async)
 (synopsis "AWS Async API for Sms")
 (depends awsm-async awsm-sms))

(package
 (name awsm-sms-voice)
 (synopsis "AWS API for Sms-voice")
 (depends awsm))

(package
 (name awsm-sms-voice-async)
 (synopsis "AWS Async API for Sms-voice")
 (depends awsm-async awsm-sms-voice))

(package
 (name awsm-snow-device-management)
 (synopsis "AWS API for Snow-device-management")
 (depends awsm))

(package
 (name awsm-snow-device-management-async)
 (synopsis "AWS Async API for Snow-device-management")
 (depends awsm-async awsm-snow-device-management))

(package
 (name awsm-snowball)
 (synopsis "AWS API for Snowball")
 (depends awsm))

(package
 (name awsm-snowball-async)
 (synopsis "AWS Async API for Snowball")
 (depends awsm-async awsm-snowball))

(package
 (name awsm-sns)
 (synopsis "AWS API for Sns")
 (depends awsm))

(package
 (name awsm-sns-async)
 (synopsis "AWS Async API for Sns")
 (depends awsm-async awsm-sns))

(package
 (name awsm-sqs)
 (synopsis "AWS API for Sqs")
 (depends awsm))

(package
 (name awsm-sqs-async)
 (synopsis "AWS Async API for Sqs")
 (depends awsm-async awsm-sqs))

(package
 (name awsm-ssm)
 (synopsis "AWS API for Ssm")
 (depends awsm))

(package
 (name awsm-ssm-async)
 (synopsis "AWS Async API for Ssm")
 (depends awsm-async awsm-ssm))

(package
 (name awsm-ssm-contacts)
 (synopsis "AWS API for Ssm-contacts")
 (depends awsm))

(package
 (name awsm-ssm-contacts-async)
 (synopsis "AWS Async API for Ssm-contacts")
 (depends awsm-async awsm-ssm-contacts))

(package
 (name awsm-ssm-incidents)
 (synopsis "AWS API for Ssm-incidents")
 (depends awsm))

(package
 (name awsm-ssm-incidents-async)
 (synopsis "AWS Async API for Ssm-incidents")
 (depends awsm-async awsm-ssm-incidents))

(package
 (name awsm-sso)
 (synopsis "AWS API for Sso")
 (depends awsm))

(package
 (name awsm-sso-async)
 (synopsis "AWS Async API for Sso")
 (depends awsm-async awsm-sso))

(package
 (name awsm-sso-admin)
 (synopsis "AWS API for Sso-admin")
 (depends awsm))

(package
 (name awsm-sso-admin-async)
 (synopsis "AWS Async API for Sso-admin")
 (depends awsm-async awsm-sso-admin))

(package
 (name awsm-sso-oidc)
 (synopsis "AWS API for Sso-oidc")
 (depends awsm))

(package
 (name awsm-sso-oidc-async)
 (synopsis "AWS Async API for Sso-oidc")
 (depends awsm-async awsm-sso-oidc))

(package
 (name awsm-stepfunctions)
 (synopsis "AWS API for Stepfunctions")
 (depends awsm))

(package
 (name awsm-stepfunctions-async)
 (synopsis "AWS Async API for Stepfunctions")
 (depends awsm-async awsm-stepfunctions))

(package
 (name awsm-storagegateway)
 (synopsis "AWS API for Storagegateway")
 (depends awsm))

(package
 (name awsm-storagegateway-async)
 (synopsis "AWS Async API for Storagegateway")
 (depends awsm-async awsm-storagegateway))

(package
 (name awsm-sts)
 (synopsis "AWS API for Sts")
 (depends awsm))

(package
 (name awsm-sts-async)
 (synopsis "AWS Async API for Sts")
 (depends awsm-async awsm-sts))

(package
 (name awsm-support)
 (synopsis "AWS API for Support")
 (depends awsm))

(package
 (name awsm-support-async)
 (synopsis "AWS Async API for Support")
 (depends awsm-async awsm-support))

(package
 (name awsm-swf)
 (synopsis "AWS API for Swf")
 (depends awsm))

(package
 (name awsm-swf-async)
 (synopsis "AWS Async API for Swf")
 (depends awsm-async awsm-swf))

(package
 (name awsm-synthetics)
 (synopsis "AWS API for Synthetics")
 (depends awsm))

(package
 (name awsm-synthetics-async)
 (synopsis "AWS Async API for Synthetics")
 (depends awsm-async awsm-synthetics))

(package
 (name awsm-textract)
 (synopsis "AWS API for Textract")
 (depends awsm))

(package
 (name awsm-textract-async)
 (synopsis "AWS Async API for Textract")
 (depends awsm-async awsm-textract))

(package
 (name awsm-timestream-query)
 (synopsis "AWS API for Timestream-query")
 (depends awsm))

(package
 (name awsm-timestream-query-async)
 (synopsis "AWS Async API for Timestream-query")
 (depends awsm-async awsm-timestream-query))

(package
 (name awsm-timestream-write)
 (synopsis "AWS API for Timestream-write")
 (depends awsm))

(package
 (name awsm-timestream-write-async)
 (synopsis "AWS Async API for Timestream-write")
 (depends awsm-async awsm-timestream-write))

(package
 (name awsm-transcribe)
 (synopsis "AWS API for Transcribe")
 (depends awsm))

(package
 (name awsm-transcribe-async)
 (synopsis "AWS Async API for Transcribe")
 (depends awsm-async awsm-transcribe))

(package
 (name awsm-transfer)
 (synopsis "AWS API for Transfer")
 (depends awsm))

(package
 (name awsm-transfer-async)
 (synopsis "AWS Async API for Transfer")
 (depends awsm-async awsm-transfer))

(package
 (name awsm-translate)
 (synopsis "AWS API for Translate")
 (depends awsm))

(package
 (name awsm-translate-async)
 (synopsis "AWS Async API for Translate")
 (depends awsm-async awsm-translate))

(package
 (name awsm-voice-id)
 (synopsis "AWS API for Voice-id")
 (depends awsm))

(package
 (name awsm-voice-id-async)
 (synopsis "AWS Async API for Voice-id")
 (depends awsm-async awsm-voice-id))

(package
 (name awsm-waf)
 (synopsis "AWS API for Waf")
 (depends awsm))

(package
 (name awsm-waf-async)
 (synopsis "AWS Async API for Waf")
 (depends awsm-async awsm-waf))

(package
 (name awsm-waf-regional)
 (synopsis "AWS API for Waf-regional")
 (depends awsm))

(package
 (name awsm-waf-regional-async)
 (synopsis "AWS Async API for Waf-regional")
 (depends awsm-async awsm-waf-regional))

(package
 (name awsm-wafv2)
 (synopsis "AWS API for Wafv2")
 (depends awsm))

(package
 (name awsm-wafv2-async)
 (synopsis "AWS Async API for Wafv2")
 (depends awsm-async awsm-wafv2))

(package
 (name awsm-wellarchitected)
 (synopsis "AWS API for Wellarchitected")
 (depends awsm))

(package
 (name awsm-wellarchitected-async)
 (synopsis "AWS Async API for Wellarchitected")
 (depends awsm-async awsm-wellarchitected))

(package
 (name awsm-wisdom)
 (synopsis "AWS API for Wisdom")
 (depends awsm))

(package
 (name awsm-wisdom-async)
 (synopsis "AWS Async API for Wisdom")
 (depends awsm-async awsm-wisdom))

(package
 (name awsm-workdocs)
 (synopsis "AWS API for Workdocs")
 (depends awsm))

(package
 (name awsm-workdocs-async)
 (synopsis "AWS Async API for Workdocs")
 (depends awsm-async awsm-workdocs))

(package
 (name awsm-worklink)
 (synopsis "AWS API for Worklink")
 (depends awsm))

(package
 (name awsm-worklink-async)
 (synopsis "AWS Async API for Worklink")
 (depends awsm-async awsm-worklink))

(package
 (name awsm-workmail)
 (synopsis "AWS API for Workmail")
 (depends awsm))

(package
 (name awsm-workmail-async)
 (synopsis "AWS Async API for Workmail")
 (depends awsm-async awsm-workmail))

(package
 (name awsm-workspaces)
 (synopsis "AWS API for Workspaces")
 (depends awsm))

(package
 (name awsm-workspaces-async)
 (synopsis "AWS Async API for Workspaces")
 (depends awsm-async awsm-workspaces))

(package
 (name awsm-workspaces-web)
 (synopsis "AWS API for Workspaces-web")
 (depends awsm))

(package
 (name awsm-workspaces-web-async)
 (synopsis "AWS Async API for Workspaces-web")
 (depends awsm-async awsm-workspaces-web))

(package
 (name awsm-xray)
 (synopsis "AWS API for Xray")
 (depends awsm))

(package
 (name awsm-xray-async)
 (synopsis "AWS Async API for Xray")
 (depends awsm-async awsm-xray))
