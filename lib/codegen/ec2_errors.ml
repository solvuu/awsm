open! Core
open! Import

(* Unlike other AWS services, EC2 endpoints can return a much wider set of errors
   than what's listed in the botodata service-2.json file.

   The only apparent list of these errors is this page, which we copy/paste below.

   https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html *)

type t =
  { name : string
  ; description : string
  }

let generate =
  Memo.general (fun text_blob ->
    text_blob
    |> String.strip
    |> String.split ~on:'\n'
    |> List.filter_map ~f:(fun line ->
         match String.lsplit2 line ~on:' ' with
         | Some (name, description) ->
           Some { name = String.strip name; description = String.strip description }
         | None -> None)
    |> List.sort ~compare:(fun a b -> String.compare a.name b.name))
;;

module Common_client_errors = struct
  let text_blob =
    {|
AuthFailure The provided credentials could not be validated. You may not be authorized to carry out the request; for example, associating an Elastic IP address that is not yours, or trying to use an AMI for which you do not have permissions. Ensure that your account is authorized to use the Amazon EC2 service, that your credit card details are correct, and that you are using the correct access keys.
Blocked Your account is currently blocked. Contact aws-verification@amazon.com if you have questions.
DryRunOperation The user has the required permissions, so the request would have succeeded, but the DryRun parameter was used.
IdempotentParameterMismatch The request uses the same client token as a previous, but non-identical request. Do not reuse a client token with different requests, unless the requests are identical.
IncompleteSignature The request signature does not conform to AWS standards.
InvalidAction The action or operation requested is not valid. Verify that the action is typed correctly.
InvalidCharacter A specified character is invalid.
InvalidClientTokenId The X.509 certificate or AWS access key ID provided does not exist in our records.
InvalidPaginationToken The specified pagination token is not valid or is expired.
InvalidParameter A parameter specified in a request is not valid, is unsupported, or cannot be used. The returned message provides an explanation of the error value. For example, if you are launching an instance, you can't specify a security group and subnet that are in different VPCs.
InvalidParameterCombination Indicates an incorrect combination of parameters, or a missing parameter. For example, trying to terminate an instance without specifying the instance ID.
InvalidParameterValue A value specified in a parameter is not valid, is unsupported, or cannot be used. Ensure that you specify a resource by using its full ID. The returned message provides an explanation of the error value.
InvalidQueryParameter The AWS query string is malformed or does not adhere to AWS standards.
MalformedQueryString The query string contains a syntax error.
MissingAction The request is missing an action or a required parameter.
MissingAuthenticationToken The request must contain either a valid (registered) AWS access key ID or X.509 certificate.
MissingParameter The request is missing a required parameter. Ensure that you have supplied all the required parameters for the request; for example, the resource ID.
OptInRequired You are not authorized to use the requested service. Ensure that you have subscribed to the service you are trying to use. If you are new to AWS, your account might take some time to be activated while your credit card details are being verified.
PendingVerification Your account is pending verification. Until the verification process is complete, you may not be able to carry out requests with this account. If you have questions, contact AWS Support.
RequestExpired The request reached the service more than 15 minutes after the date stamp on the request or more than 15 minutes after the request expiration date (such as for presigned URLs), or the date stamp on the request is more than 15 minutes in the future. If you're using temporary security credentials, this error can also occur if the credentials have expired. For more information, see Temporary Security Credentials in the IAM User Guide.
UnauthorizedOperation You are not authorized to perform this operation. Check your IAM policies, and ensure that you are using the correct access keys. For more information, see Controlling Access. If the returned message is encoded, you can decode it using the DecodeAuthorizationMessage action. For more information, see DecodeAuthorizationMessage in the AWS Security Token Service API Reference.
UnknownParameter An unknown or unrecognized parameter was supplied. Requests that could cause this error include supplying a misspelled parameter or a parameter that is not supported for the specified API version.
UnsupportedInstanceAttribute The specified attribute cannot be modified.
UnsupportedOperation The specified request includes an unsupported operation. For example, you can't stop an instance that's instance store-backed. Or you might be trying to launch an instance type that is not supported by the specified AMI. The returned message provides details of the unsupported operation.
UnsupportedProtocol SOAP has been deprecated and is no longer supported. For more information, see SOAP Requests.
ValidationError The input fails to satisfy the constraints specified by an AWS service.
|}
  ;;

  let enumerate () = generate text_blob
end

module Client_errors_for_specific_actions = struct
  let text_blob =
    {|
ActiveVpcPeeringConnectionPerVpcLimitExceeded You've reached the limit on the number of active VPC peering connections you can have for the specified VPC.
AddressLimitExceeded You've reached the limit on the number of Elastic IP addresses that you can allocate.  For more information, see Elastic IP Address Limit. If you need additional Elastic IP addresses, complete the Amazon EC2 Elastic IP Address Request Form. If you need additional Elastic IP addresses for your VPCs, complete the Amazon VPC Limits form.
AsnConflict The Autonomous System Numbers (ASNs) of the specified customer gateway and the specified virtual private gateway are the same.
AttachmentLimitExceeded You've reached the limit on the number of Amazon EBS volumes or network interfaces that can be attached to a single instance.
BootForVolumeTypeUnsupported The specified volume type cannot be used as a boot volume. For more information, see Amazon EBS Volume Types.
BundlingInProgress The specified instance already has a bundling task in progress.
CannotDelete You cannot delete the 'default' security group in your VPC, but you can change its rules. For more information, see Amazon EC2 Security Groups.
ClientVpnAuthorizationRuleLimitExceeded You've reached the limit on the number of authorization rules that can be added to a single Client VPN endpoint.
ClientVpnCertificateRevocationListLimitExceeded You've reached the limit on the number of client certificate revocation lists that can be added to a single Client VPN endpoint.
ClientVpnEndpointAssociationExists The specified target network is already associated with the Client VPN endpoint.
ClientVpnEndpointLimitExceeded You've reached the limit on the number of Client VPN endpoints that you can create.
ClientVpnRouteLimitExceeded You've reached the limit on the number of routes that can be added to a single Client VPN endpoint.
ClientVpnTerminateConnectionsLimitExceeded The number of client connections you're attempting to terminate exceeds the limit.
CidrConflict You cannot enable a VPC for ClassicLink or extend a VPC peering connection to use the ClassicLink connection of a peer VPC if the VPC has routing that conflicts with the EC2-Classic private IP address range.
ConcurrentCreateImageNoRebootLimitExceeded The maximum number of concurrent CreateImage requests for the instance has been reached. Wait for the current CreateImage requests to complete, and then retry your request.
ConcurrentSnapshotLimitExceeded You've reached the limit on the number of concurrent snapshots you can create on the specified volume. Wait until the 'pending' requests have completed, and check that you do not have snapshots that are in an incomplete state, such as 'error', which count against your concurrent snapshot limit.
ConcurrentTagAccess You can't run simultaneous commands to modify a tag for a specific resource. Allow sufficient wait time for the previous request to complete, then retry your request. For more information, see Error Retries and Exponential Backoff in AWS.
CreditSpecificationUpdateInProgress The default credit specification for the instance family is currently being updated. It takes about five minutes to complete. For more information, see Setting the Default Credit Specification for the Account.
CustomerGatewayLimitExceeded You've reached the limit on the number of customer gateways you can create for the AWS Region. For more information, see Amazon VPC Limits. To request an increase on your customer gateway limit, complete the Amazon VPC Limits form.
CustomerKeyHasBeenRevoked The customer master key cannot be accessed. For more information, see Amazon EBS Encryption.
DeleteConversionTaskError The conversion task cannot be canceled.
DefaultSubnetAlreadyExistsInAvailabilityZone A default subnet already exists in the specified Availability Zone. You can have only one default subnet per Availability Zone.
DefaultVpcAlreadyExists A default VPC already exists in the AWS Region. You can only have one default VPC per Region.
DefaultVpcDoesNotExist There is no default VPC in which to carry out the request. If you've deleted your default VPC, you can create a new one. For more information, see Creating a Default VPC.
DependencyViolation The specified object has dependent resources. A number of resources in a VPC may have dependent resources, which prevent you from deleting or detaching them. Remove the dependencies first, then retry your request. For example, this error occurs if you try to delete a security group in a VPC that is in use by another security group.
DisallowedForDedicatedTenancyNetwork Dedicated tenancy VPCs cannot be used with ClassicLink. If you want to allow your dedicated tenancy VPC to be enabled for ClassicLink, contact AWS Support.
DiskImageSizeTooLarge The disk image exceeds the allowed limit (for instance or volume import).
DuplicateSubnetsInSameZone For an interface VPC endpoint, you can specify only one subnet per Availability Zone.
EIPMigratedToVpc The Elastic IP address has been migrated to EC2-VPC, and cannot be used in EC2-Classic.
EncryptedVolumesNotSupported Encrypted Amazon EBS volumes may only be attached to instances that support Amazon EBS encryption. For more information, see Amazon EBS encryption in the Amazon EC2 User Guide for Linux Instances.
ExistingVpcEndpointConnections You cannot delete a VPC endpoint service configuration or change the load balancers for the endpoint service if there are endpoints attached to the service.
FleetNotInModifiableState The Spot Fleet request must be in the active state in order to modify it. For more information, see Modifying a Spot Fleet Request.
FlowLogAlreadyExists A flow log with the specified configuration already exists.
FlowLogsLimitExceeded You've reached the limit on the number of flow logs you can create. For more information, see Amazon VPC Limits.
FilterLimitExceeded The request uses too many filters or too many filter values.
Gateway.NotAttached An internet gateway is not attached to a VPC. If you are trying to detach an internet gateway, ensure that you specify the correct VPC. If you are trying to associate an Elastic IP address with a network interface or an instance, ensure that an internet gateway is attached to the relevant VPC.
HostAlreadyCoveredByReservation The specified Dedicated Host is already covered by a reservation.
HostLimitExceeded You've reached the limit on the number of Dedicated Hosts that you can allocate. For more information, see Dedicated Hosts.
IdempotentInstanceTerminated The request to launch an instance uses the same client token as a previous request for which the instance has been terminated.
InaccessibleStorageLocation The specified Amazon S3 URL cannot be accessed. Check the access permissions for the URL.
IncorrectInstanceState The instance is in an incorrect state for the requested action. For example, some instance attributes, such as user data, can only be modified if the instance is in a 'stopped' state.  If you are associating an Elastic IP address with a network interface, ensure that the instance that the interface is attached to is not in the 'pending' state.
IncorrectModificationState A new modification action on an EBS Elastic Volume cannot occur because the volume is currently being modified.
IncorrectState The resource is in an incorrect state for the request. This error can occur if you are trying to attach a volume that is still being created. Ensure that the volume is in the 'available' state. If you are creating a snapshot, ensure that the previous request to create a snapshot on the same volume has completed. If you are deleting a virtual private gateway, ensure that it's detached from the VPC.
IncompatibleHostRequirements There are no available or compatible Dedicated Hosts available on which to launch or start the instance.
InstanceAlreadyLinked The EC2-Classic instance you are trying to link is already linked to another VPC. You cannot link an EC2-Classic instance to more than one VPC at a time.
InstanceCreditSpecification.NotSupported The specified instance does not use CPU credits for CPU usage; only T2 instances use CPU credits for CPU usage.
InstanceLimitExceeded You've reached the limit on the number of instances you can run concurrently. This error can occur if you are launching an instance or if you are creating a Capacity Reservation. Capacity Reservations count towards your On-Demand Instance limits. If your request fails due to limit constraints, increase your On-Demand Instance limit for the required instance type and try again. For more information, see How many instances can I run in Amazon EC2. If you need additional instances, complete the Amazon EC2 Instance Request Form.
InsufficientCapacityOnHost There is not enough capacity on the Dedicated Host to launch or start the instance.
InsufficientFreeAddressesInSubnet The specified subnet does not contain enough free private IP addresses to fulfill your request. Use the DescribeSubnets request to view how many IP addresses are available (unused) in your subnet. IP addresses associated with stopped instances are considered unavailable.
InsufficientReservedInstancesCapacity There is insufficient capacity for the requested Reserved Instances.
InterfaceInUseByTrafficMirrorSession The Traffic Mirror source that you are trying to create uses an interface that is already associated with a session. An interface can only be associated with a session, or with a target, but not both.
InterfaceInUseByTrafficMirrorTarget The Traffic Mirror source that you are trying to create uses an interface that is already associated with a target. An interface can only be associated with a session, or with a target, but not both. If the interface is associated with a target, it cannot be associated with another target.
InternetGatewayLimitExceeded You've reached the limit on the number of internet gateways that you can create. For more information, see Amazon VPC Limits. To request an increase on the internet gateway limit, complete the Amazon VPC Limits form.
InvalidAddress.Locked The specified Elastic IP address cannot be released from your account. A reverse DNS record may be associated with the Elastic IP address. To unlock the address, contact AWS Support.
InvalidAddress.Malformed The specified IP address is not valid. Ensure that you provide the address in the form xx.xx.xx.xx; for example, 55.123.45.67
InvalidAddress.NotFound The specified Elastic IP address that you are describing cannot be found. Ensure that you specify the AWS Region in which the IP address is located, if it's not in the default Region.
InvalidAddressID.NotFound The specified allocation ID for the Elastic IP address you are trying to release cannot be found. Ensure that you specify the AWS Region in which the IP address is located, if it's not in the default Region.
InvalidAffinity The specified affinity value is not valid.
InvalidAllocationID.NotFound The specified allocation ID you are trying to describe or associate does not exist. Ensure that you specify the AWS Region in which the IP address is located, if it's not in the default Region.
InvalidAMIAttributeItemValue The value of an item added to, or removed from, an image attribute is not valid. If you are specifying a userId, check that it is in the form of an AWS account ID, without hyphens.
InvalidAMIID.Malformed The specified AMI ID is malformed. Ensure that you provide the full AMI ID, in the form ami-xxxxxxxx.
InvalidAMIID.NotFound The specified AMI does not exist. Check the AMI ID, and ensure that you specify the AWS Region in which the AMI is located, if it's not in the default Region. This error may also occur if you specified an incorrect kernel ID when launching an instance.
InvalidAMIID.Unavailable The specified AMI has been deregistered and is no longer available, or is not in a state from which you can launch an instance or modify attributes.
InvalidAMIName.Duplicate The specified AMI name is already in use by another AMI. If you have recently deregistered an AMI with the same name, allow enough time for the change to propagate through the system, and retry your request.
InvalidAMIName.Malformed AMI names must be between 3 and 128 characters long, and may only contain letters, numbers, and the following special characters: '-', '_', '.', '/', '(', and ')'.
InvalidAssociationID.NotFound The specified association ID (for an Elastic IP address, a route table, or network ACL) does not exist. Ensure that you specify the AWS Region in which the association ID is located, if it's not in the default Region.
InvalidAttachment.NotFound Indicates an attempt to detach a volume from an instance to which it is not attached.
InvalidAttachmentID.NotFound The specified network interface attachment does not exist.
InvalidAutoPlacement The specified value for auto-placement is not valid.
InvalidAvailabilityZone The specified Availability Zone is not valid.
InvalidBlockDeviceMapping A block device mapping parameter is not valid. The returned message indicates the incorrect value.
InvalidBundleID.NotFound The specified bundle task ID cannot be found. Ensure that you specify the AWS Region in which the bundle task is located, if it's not in the default Region.
InvalidCidr.InUse The specified inside tunnel CIDR is already in use by another VPN tunnel for the virtual private gateway.
InvalidClientToken The specified client token is not valid. For more information, see Idempotency in Amazon EC2.
InvalidClientVpnAssociationIdNotFound The specified target network association cannot be found.
InvalidClientVpnConnection.IdNotFound The specified Client VPN endpoint cannot be found.
InvalidClientVpnConnection.UserNotFound The specified user does not have an active connection to the specified Client VPN endpoint.
InvalidClientVpnDuplicateAssociationException The specified target network has already been associated with the Client VPN endpoint.
InvalidClientVpnDuplicateAuthorizationRule The specified authorization has already been added to the Client VPN endpoint.
InvalidClientVpnDuplicateRoute The specified route has already been added to the Client VPN endpoint.
InvalidClientVpnEndpointAuthorizationRuleNotFound The specified authorization rule cannot be found.
InvalidClientVpnRouteNotFound The specified route cannot be found.
InvalidClientVpnSubnetId.DifferentAccount The specified subnet belongs to a different account.
InvalidClientVpnSubnetId.DuplicateAz You have already associated a subnet from this Availability Zone with the Client VPN endpoint.
InvalidClientVpnSubnetId.NotFound The specified subnet cannot be found in the VPN with which the Client VPN endpoint is associated.
InvalidClientVpnSubnetId.OverlappingCidr The specified target network's CIDR range overlaps with the Client VPN endpoint's client CIDR range.
InvalidClientVpnActiveAssociationNotFound You cannot perform this action on the Client VPN endpoint while it is in the pending-association state.
InvalidClientVpnEndpointId.NotFound The specified Client VPN Endpoint cannot be found.
InvalidConversionTaskId The specified conversion task ID (for instance or volume import) is not valid.
InvalidConversionTaskId.Malformed The specified conversion task ID (for instance or volume import) is malformed. Ensure that you've specified the ID in the form import-i-xxxxxxxx.
InvalidCpuCredits.Malformed The specified CpuCredit value is invalid. Valid values are standard and unlimited.
InvalidCustomerGateway.DuplicateIpAddress There is a conflict among the specified gateway IP addresses. Each VPN connection in an AWS Region must be created with a unique customer gateway IP address (across all AWS accounts). For more information, see Your Customer Gateway in the AWS Site-to-Site VPN Network Administrator Guide.
InvalidCustomerGatewayId.Malformed The specified customer gateway ID is malformed, or cannot be found. Specify the ID in the form cgw-xxxxxxxx, and ensure that you specify the AWS Region in which the customer gateway is located, if it's not in the default Region.
InvalidCustomerGatewayID.NotFound The specified customer gateway ID cannot be found. Ensure that you specify the AWS Region in which the customer gateway is located, if it's not in the default Region.
InvalidCustomerGatewayState The customer gateway is not in the available state, and therefore cannot be used.
InvalidDevice.InUse The device to which you are trying to attach (for example, /dev/sdh) is already in use on the instance.
InvalidDhcpOptionID.NotFound The specified DHCP options set does not exist. Ensure that you specify the AWS Region in which the DHCP options set is located, if it's not in the default Region.
InvalidDhcpOptionsID.NotFound The specified DHCP options set does not exist. Ensure that you specify the AWS Region in which the DHCP options set is located, if it's not in the default Region.
InvalidDhcpOptionsId.Malformed The specified DHCP options set ID is malformed. Ensure that you provide the full DHCP options set ID in the request, in the form dopt-xxxxxxxx.
InvalidExportTaskID.NotFound The specified export task ID cannot be found.
InvalidFilter The specified filter is not valid.
InvalidFlowLogId.NotFound The specified flow log does not exist. Ensure that you have indicated the AWS Region in which the flow log is located, if it's not in the default Region.
InvalidFormat The specified disk format (for the instance or volume import) is not valid.
InvalidFpgaImageID.Malformed The specified Amazon FPGA image (AFI) ID is malformed. Ensure that you provide the full AFI ID in the request, in the form afi-xxxxxxxxxxxxxxxxx.
InvalidFpgaImageID.NotFound The specified Amazon FPGA image (AFI) ID does not exist. Ensure that you specify the AWS Region in which the AFI is located, if it's not in the default Region.
InvalidGatewayID.NotFound The specified gateway does not exist.
InvalidGroup.Duplicate You cannot create a security group with the same name as an existing security group in the same VPC, or the same AWS Region (EC2-Classic).
InvalidGroupId.Malformed The specified security group ID is malformed. Ensure that you provide the full security group ID in the request, in the form sg-xxxxxxxx.
InvalidGroup.InUse The specified security group can't be deleted because it's in use by another security group. You can remove dependencies by modifying or deleting rules in the affected security groups.
InvalidGroup.NotFound The specified security group does not exist.  This error can occur because the ID of a recently created security group has not propagated through the system. For more information, see Eventual consistency.  You can't specify a security group that is in a different AWS Region or VPC than the request.
InvalidGroup.Reserved The name 'default' is reserved, and cannot be used to create a new security group. You also cannot delete the default EC2-Classic security group, but you can change its rules. For more information, see Amazon EC2 Security Groups.
InvalidHostConfiguration The specified Dedicated Host configuration is not supported.
InvalidHostId The specified Dedicated Host ID is not valid.
InvalidHostID.Malformed The specified Dedicated Host ID is not formed correctly. Ensure that you provide the full ID in the form h-xxxxxxxxxxxxxxxxx.
InvalidHostId.Malformed The specified Dedicated Host ID is not formed correctly. Ensure that you provide the full ID in the form h-xxxxxxxxxxxxxxxxx.
InvalidHostID.NotFound The specified Dedicated Host ID does not exist. Ensure that you specify the AWS Region in which the Dedicated Host is located, if it's not in the default Region.
InvalidHostId.NotFound The specified Dedicated Host ID does not exist. Ensure that you specify the region in which the Dedicated Host is located, if it's not in the default region.
InvalidHostReservationId.Malformed The specified Dedicated Host Reservation ID is not formed correctly. Ensure that you provide the full ID in the form hr-xxxxxxxxxxxxxxxxx.
InvalidHostReservationOfferingId.Malformed The specified Dedicated Host Reservation offering is not formed correctly. Ensure that you provide the full ID in the form hro-xxxxxxxxxxxxxxxxx.
InvalidHostState The Dedicated Host must be in the available state to complete the operation.
InvalidIamInstanceProfileArn.Malformed The specified IAM instance profile ARN is not valid. For more information about valid ARN formats, see Amazon Resource Names (ARNs).
InvalidID The specified ID for the resource you are trying to tag is not valid. Ensure that you provide the full resource ID; for example, ami-2bb65342 for an AMI.  If you're using the command line tools on a Windows system, you might need to use quotation marks for the key-value pair; for example, "Name=TestTag".
InvalidInput An input parameter in the request is not valid. For example, you may have specified an incorrect Reserved Instance listing ID in the request or the Reserved Instance you tried to list cannot be sold in the Reserved Instances Marketplace (for example, if it has a scope of Region, or is a Convertible Reserved Instance).
InvalidInstanceAttributeValue The specified instance attribute value is not valid. This error is most commonly encountered when trying to set the InstanceType/--instance-type attribute to an unrecognized value.
InvalidInstanceCreditSpecification.DuplicateInstanceId If you are modifying the credit option for CPU usage for T2 instances, the request may not contain duplicate instance IDs.
InvalidInstanceFamily The instance family is not supported for this request. For example, the instance family for the Dedicated Host Reservation offering is different from the instance family of the Dedicated Hosts. Or, you can only modify the default credit specification for burstable performance instance families (T2, T3, and T3a). For more information, see Setting the Default Credit Specification for the Account.
InvalidInstanceID This error occurs when trying to associate an IP address with an instance in EC2-Classic that is not in the running state. This error can also occur when trying to perform an operation on an instance that has multiple network interfaces.  A network interface can have individual attributes; therefore, you may need to specify the network interface ID as part of the request, or use a different request. For example, each network interface in an instance can have a source/destination check flag. To modify this attribute, modify the network interface attribute, and not the instance attribute.  To create a route in a route table, provide a specific network interface ID as part of the request.
InvalidInstanceID.Malformed The specified instance ID is malformed. Ensure that you provide the full instance ID in the request, in the form i-xxxxxxxx or i-xxxxxxxxxxxxxxxxx.
InvalidInstanceID.NotFound The specified instance does not exist. Ensure that you have indicated the AWS Region in which the instance is located, if it's not in the default Region. This error may occur because the ID of a recently created instance has not propagated through the system. For more information, see Eventual consistency.
InvalidInstanceID.NotLinkable The specified instance cannot be linked to the specified VPC. Ensure that the instance is an EC2-Classic instance. This error may also occur if the instance was recently launched, and its ID has not yet propagated through the system. Wait a few minutes, or wait until the instance is in the running state, and then try again.
InvalidInstanceState The instance is not in an appropriate state to complete the request. If you're modifying the instance placement, the instance must be in the stopped state.
InvalidInstanceType The instance type is not supported for this request. For example, you can only bundle instance store-backed Windows instances.
InvalidInterface.IpAddressLimitExceeded The number of private IP addresses for a specified network interface exceeds the limit for the type of instance you are trying to launch. For more information about the maximum number of private IP addresses per elastic network interface, see Private IP addresses per Elastic Network Interface.
InvalidInternetGatewayId.Malformed The specified internet gateway ID is malformed. Ensure that you provide the full ID in the request, in the form igw-xxxxxxxx.
InvalidInternetGatewayID.NotFound The specified internet gateway does not exist. Ensure that you specify the AWS Region in which the internet gateway is located, if it's not in the default Region.
InvalidIPAddress.InUse The specified IP address is already in use. If you are trying to release an address, you must first disassociate it from the instance.
InvalidKernelId.Malformed The specified kernel ID is not valid. Ensure that you specify the kernel ID in the form aki-xxxxxxxx.
InvalidKey.Format The key pair is not specified in a valid OpenSSH public key format.
InvalidKeyPair.Duplicate The key pair name already exists in that AWS Region. If you are creating or importing a key pair, ensure that you use a unique name.
InvalidKeyPair.Format The format of the public key you are attempting to import is not valid.
InvalidKeyPair.NotFound The specified key pair name does not exist. Ensure that you specify the AWS Region in which the key pair is located, if it's not in the default Region.
InvalidCapacityReservationIdMalformedException The ID for the Capacity Reservation is malformed. Ensure that you specify the Capacity Reservation ID in the form cr-xxxxxxxxxxxxxxxxx.
InvalidCapacityReservationIdNotFoundException The specified Capacity Reservation ID does not exist.
InvalidLaunchTemplateId.Malformed The ID for the launch template is malformed. Ensure that you specify the launch template ID in the form lt-xxxxxxxxxxxxxxxxx.
InvalidLaunchTemplateId.NotFound The specified launch template ID does not exist. Ensure that you specify the AWS Region in which the launch template is located.
InvalidLaunchTemplateId.VersionNotFound The specified launch template version does not exist.
InvalidLaunchTemplateName.AlreadyExistsException The specified launch template name is already in use.
InvalidLaunchTemplateName.MalformedException The specified launch template name is invalid. A launch template name must be between 3 and 128 characters, and may contain letters, numbers, and the following characters: '-', '_', '.', '/', '(', and ')'.
InvalidLaunchTemplateName.NotFoundException The specified launch template name does not exist. Check the spelling of the name and ensure that you specify the AWS Region in which the launch template is located. Launch template names are case-sensitive.
InvalidManifest The specified AMI has an unparsable manifest, or you may not have access to the location of the manifest file in Amazon S3.
InvalidMaxResults The specified value for MaxResults is not valid.
InvalidNatGatewayID.NotFound The specified NAT gateway ID does not exist. Ensure that you specify the AWS Region in which the NAT gateway is located, if it's not in the default Region.
InvalidNetworkAclEntry.NotFound The specified network ACL entry does not exist.
InvalidNetworkAclId.Malformed The specified network ACL ID is malformed. Ensure that you provide the ID in the form acl-xxxxxxxx.
InvalidNetworkAclID.NotFound The specified network ACL does not exist. Ensure that you specify the AWS Region in which the network ACL is located, if it's not in the default Region.
InvalidNetworkLoadBalancerArn.Malformed The specified Network Load Balancer ARN is malformed. Ensure that you specify the ARN in the form arn:aws:elasticloadbalancing:region:account-id:loadbalancer/net/load-balancer-name/load-balancer-id.
InvalidNetworkLoadBalancerArn.NotFound The specified Network Load Balancer ARN does not exist.
InvalidNetworkInterfaceAttachmentId.Malformed The ID for the network interface attachment is malformed. Ensure that you use the attachment ID rather than the network interface ID, in the form eni-attach-xxxxxxxx.
InvalidNetworkInterface.InUse The specified interface is currently in use and cannot be deleted or attached to another instance. Ensure that you have detached the network interface first. If a network interface is in use, you may also receive the InvalidParameterValue error.
InvalidNetworkInterfaceId.Malformed The specified network interface ID is malformed. Ensure that you specify the network interface ID in the form eni-xxxxxxxx.
InvalidNetworkInterfaceID.NotFound The specified network interface does not exist. Ensure that you specify the AWS Region in which the network interface is located, if it's not in the default Region.
InvalidNextToken The specified NextToken is not valid.
InvalidOption.Conflict A VPN connection between the virtual private gateway and the customer gateway already exists.
InvalidPermission.Duplicate The specified inbound or outbound rule already exists for that security group.
InvalidPermission.Malformed The specified security group rule is malformed. If you are specifying an IP address range, ensure that you use CIDR notation; for example, 203.0.113.0/24.
InvalidPermission.NotFound The specified rule does not exist in this security group.
InvalidPlacementGroup.Duplicate The specified placement group already exists in that AWS Region.
InvalidPlacementGroup.InUse The specified placement group is in use. If you are trying to delete a placement group, ensure that its instances have been terminated.
InvalidPlacementGroup.Unknown The specified placement group cannot be found. Ensure that you specify the AWS Region in which the placement group is located, if it's not in the default Region.
InvalidPolicyDocument The specified policy document is not a valid JSON policy document.
InvalidPrefixListId.Malformed The specified prefix list ID is malformed. Ensure that you provide the ID in the form pl-xxxxxxxx.
InvalidPrefixListId.NotFound The specified prefix list ID does not exist. Ensure that you have indicated the AWS Region for the service, if it's not in the default Region.
InvalidProductInfo (AWS Marketplace) The product code is not valid.
InvalidPurchaseToken.Expired The specified purchase token has expired.
InvalidPurchaseToken.Malformed The specified purchase token is not valid.
InvalidQuantity The specified quantity of Dedicated Hosts is not valid.
InvalidRamDiskId.Malformed The specified RAM disk ID is not valid. Ensure that you specify the RAM disk ID in the form ari-xxxxxxxx.
InvalidRegion The specified AWS Region is not valid. For copying a snapshot or image, specify the source Region using its Region code, for example, us-west-2.
InvalidRequest The request is not valid. The returned message provides details about the nature of the error.
InvalidReservationID.Malformed The specified reservation ID is not valid.
InvalidReservationID.NotFound The specified reservation does not exist.
InvalidReservedInstancesId The specified Reserved Instance does not exist.
InvalidReservedInstancesOfferingId The specified Reserved Instances offering does not exist.
InvalidResourceType.Unknown The specified resource type is not supported or is not valid. To view resource types that support longer IDs, use DescribeIdFormat.
InvalidRoute.InvalidState The specified route is not valid.
InvalidRoute.Malformed The specified route is not valid. If you are deleting a route in a VPN connection, ensure that you've entered the value for the CIDR block correctly.
InvalidRoute.NotFound The specified route does not exist in the specified route table. Ensure that you indicate the exact CIDR range for the route in the request. This error can also occur if you've specified a route table ID in the request that does not exist.
InvalidRouteTableId.Malformed The specified route table ID is malformed. Ensure that you specify the route table ID in the form rtb-xxxxxxxx.
InvalidRouteTableID.NotFound The specified route table does not exist. Ensure that you specify the AWS Region in which the route table is located, if it's not in the default Region.
InvalidScheduledInstance The specified Scheduled Instance does not exist.
InvalidSecurityGroupId.Malformed The specified security group ID is not valid. Ensure that you specify the security group ID in the form sg-xxxxxxxx.
InvalidSecurityGroupID.NotFound The specified security group does not exist. If you are creating a network interface, ensure that you specify a VPC security group, and not an EC2-Classic security group.
InvalidSecurity.RequestHasExpired The difference between the request timestamp and the AWS server time is greater than 5 minutes. Ensure that your system clock is accurate and configured to use the correct time zone.
InvalidServiceName The name of the service is not valid. To get a list of available service names, use DescribeVpcEndpointServices.
InvalidSnapshotID.Malformed The snapshot ID is not valid.
InvalidSnapshot.InUse The snapshot that you are trying to delete is in use by one or more AMIs.
InvalidSnapshot.NotFound The specified snapshot does not exist. Ensure that you specify the AWS Region in which the snapshot is located, if it's not in the default Region.
InvalidSpotDatafeed.NotFound You have no data feed for Spot Instances.
InvalidSpotFleetRequestConfig The Spot Fleet request configuration is not valid. Ensure that you provide valid values for all of the configuration parameters; for example, a valid AMI ID. Limits apply on the target capacity and the number of launch specifications per Spot Fleet request. For more information, see Spot Fleet Limits.
InvalidSpotFleetRequestId.Malformed The specified Spot Fleet request ID is malformed. Ensure that you specify the Spot Fleet request ID in the form sfr- followed by 36 characters, including hyphens; for example, sfr-123f8fc2-11aa-22bb-33cc-example12710.
InvalidSpotFleetRequestId.NotFound The specified Spot Fleet request ID does not exist. Ensure that you specify the AWS Region in which the Spot Fleet request is located, if it's not in the default Region.
InvalidSpotInstanceRequestID.Malformed The specified Spot Instance request ID is not valid. Ensure that you specify the Spot Instance request ID in the form sir-xxxxxxxx.
InvalidSpotInstanceRequestID.NotFound The specified Spot Instance request ID does not exist. Ensure that you specify the AWS Region in which the Spot Instance request is located, if it's not in the default Region.
InvalidState The specified resource is not in the correct state for the request; for example, if you are trying to enable monitoring on a recently terminated instance, or if you are trying to create a snapshot when a previous identical request has not yet completed.
InvalidStateTransition The specified VPC peering connection is not in the correct state for the request. For example, you may be trying to accept a VPC peering request that has failed, or that was rejected.
InvalidSubnet The specified subnet ID is not valid or does not exist.
InvalidSubnet.Conflict The specified CIDR block conflicts with that of another subnet in your VPC.
InvalidSubnetID.Malformed The specified subnet ID is malformed. Ensure that you specify the ID in the form subnet-xxxxxxxx
InvalidSubnetID.NotFound The specified subnet does not exist. Ensure that you have indicated the AWS Region in which the subnet is located, if it's not in the default Region.
InvalidSubnet.Range The CIDR block you've specified for the subnet is not valid. The allowed block size is between a /28 netmask and /16 netmask.
InvalidTagKey.Malformed The specified tag key is not valid. Tag keys cannot be empty or null, and cannot start with aws:.
InvalidTargetArn.Unknown The specified ARN for the IAM user, IAM role, or root user is not valid or does not exist.
InvalidTenancy The tenancy of the instance or VPC is not supported for the requested action. For example, you cannot modify the tenancy of an instance or VPC that has a tenancy attribute of default.
InvalidTime The specified timestamp is not valid.
InvalidTrafficMirrorFilterNotFound The specified Traffic Mirror filter does not exist.
InvalidTrafficMirrorFilterRuleNotFound The specified Traffic Mirror filter rule does not exist.
InvalidTrafficMirrorSessionNotFound The specified Traffic Mirror session does not exist.
InvalidTrafficMirrorTargetNoFound The specified Traffic Mirror target does not exist.
InvalidUserID.Malformed The specified user or owner is not valid. If you are performing a DescribeImages request, you must specify a valid value for the owner or executableBy parameters, such as an AWS account ID. If you are performing a DescribeSnapshots request, you must specify a valid value for the owner or restorableBy parameters.
InvalidVolumeID.Duplicate The Amazon EBS volume already exists.
InvalidVolumeID.Malformed The specified volume ID is not valid. Check the letter-number combination carefully.
InvalidVolumeID.ZoneMismatch The specified volume and instance are in different Availability Zones.
InvalidVolume.NotFound The specified volume does not exist. Ensure that you have indicated the AWS Region in which the volume is located, if it's not in the default Region. Ensure that you are using the correct access credentials.
InvalidVolume.ZoneMismatch The specified volume is not in the same Availability Zone as the specified instance. You can only attach an Amazon EBS volume to an instance if they are in the same Availability Zone.
InvalidVpcEndpointId.Malformed The specified VPC endpoint ID is malformed. Use the full VPC endpoint ID in the request, in the form vpce-xxxxxxxx.
InvalidVpcEndpoint.NotFound The specified VPC endpoint does not exist. If you are performing a bulk request that is partially successful or unsuccessful, the response includes a list of the unsuccessful items. If the request succeeds, the list is empty.
InvalidVpcEndpointId.NotFound The specified VPC endpoint does not exist. If you are performing a bulk request that is partially successful or unsuccessful, the response includes a list of the unsuccessful items. If the request succeeds, the list is empty.
InvalidVpcEndpointService.NotFound The specified VPC endpoint service does not exist. If you are performing a bulk request that is partially successful or unsuccessful, the response includes a list of the unsuccessful items. If the request succeeds, the list is empty.
InvalidVpcEndpointServiceId.NotFound The specified VPC endpoint service does not exist. If you are performing a bulk request that is partially successful or unsuccessful, the response includes a list of the unsuccessful items. If the request succeeds, the list is empty.
InvalidVpcEndpointType The specified VPC endpoint type is not valid. Valid values are Interface and Gateway.
InvalidVpcID.Malformed The specified VPC ID is malformed. Ensure that you've specified the ID in the form vpc-xxxxxxxx.
InvalidVpcID.NotFound The specified VPC does not exist. Ensure that you have indicated the AWS Region in which the VPC is located, if it's not in the default Region.
InvalidVpcPeeringConnectionId.Malformed The specified VPC peering connection ID is malformed. Ensure that you provide the ID in the form pcx-xxxxxxxx.
InvalidVpcPeeringConnectionID.NotFound The specified VPC peering connection ID does not exist. Ensure that you have indicated the AWS Region in which the VPC peering connection is located, if it's not in the default Region.
InvalidVpcPeeringConnectionState.DnsHostnamesDisabled To enable DNS hostname resolution for the VPC peering connection, DNS hostname support must be enabled for the VPCs.
InvalidVpcRange The specified CIDR block range is not valid. The block range must be between a /28 netmask and /16 netmask. For more information, see Your VPC and Subnets.
InvalidVpcState The specified VPC already has a virtual private gateway attached to it.
InvalidVpnConnectionID The specified VPN connection ID cannot be found. Ensure that you have indicated the AWS Region in which the VPN connection ID is located, if it's not in the default Region.
InvalidVpnConnectionID.NotFound The specified VPN connection ID does not exist. Ensure that you have indicated the AWS Region in which the VPN connection ID is located, if it's not in the default Region.
InvalidVpnConnection.InvalidState The VPN connection must be in the available state to complete the request.
InvalidVpnConnection.InvalidType The specified VPN connection does not support static routes.
InvalidVpnGatewayAttachment.NotFound An attachment between the specified virtual private gateway and specified VPC does not exist. This error can also occur if you've specified an incorrect VPC ID in the request.
InvalidVpnGatewayID.NotFound The specified virtual private gateway does not exist. Ensure that you have indicated the AWS Region in which the virtual private gateway is located, if it's not in the default Region.
InvalidVpnGatewayState The virtual private gateway is not in an available state.
InvalidZone.NotFound The specified Availability Zone does not exist, or is not available for you to use. Use the DescribeAvailabilityZones request to list the Availability Zones that are currently available to you. Ensure that you have indicated the AWS Region for the Availability Zone in the request, if it's not in the default Region. Specify the full name of the Availability Zone: for example, us-east-1a.
KeyPairLimitExceeded You've reached the limit on the number of key pairs that you can have in this AWS Region. For more information, see Amazon EC2 Key Pairs.
LegacySecurityGroup Any VPC created using an API version older than 2011-01-01 may have the 2009-07-15-default security group. You must delete this security group before you can attach an internet gateway to the VPC.
LimitPriceExceeded The cost of the total order is greater than the specified limit price (instance count * price).
LogDestinationNotFoundException The specified Amazon S3 bucket does not exist. Ensure that you have specified the ARN for an existing Amazon S3 bucket, and that the ARN is in the correct format.
LogDestinationPermissionIssue You do not have sufficient permissions to publish flow logs to the specific Amazon S3 bucket.
MaxConfigLimitExceededException You’ve exceeded your maximum allowed Spot placement configurations. You can retry configurations that you used within the last 24 hours, or wait for 24 hours before specifying a new configuration. For more information, see Spot placement score.
MaxIOPSLimitExceeded You've reached the limit on your IOPS usage for that AWS Region. To increase your volume limit, complete the Amazon EC2 EBS Volume Limit Form.
MaxScheduledInstanceCapacityExceeded You've attempted to launch more instances than you purchased.
MaxSpotFleetRequestCountExceeded You've reached one or both of these limits: the total number of Spot Fleet requests that you can make, or the total number of instances in all Spot Fleets for the AWS Region (the target capacity). For more information, see Fleet quotas.
MaxSpotInstanceCountExceeded You've reached the limit on the number of Spot Instances that you can launch. The limit depends on the instance type. For more information, see How many instances can I run in Amazon EC2. If you need additional instances, complete the Amazon EC2 Instance Request Form.
MaxTemplateLimitExceeded You've reached the limit on the number of launch templates you can create. For more information, see Launch Template Restrictions .
MaxTemplateVersionLimitExceeded You've reached the limit on the number of launch template versions you can create. For more information, see Launch Template Restrictions .
MissingInput An input parameter is missing.
NatGatewayLimitExceeded You've reached the limit on the number of NAT gateways that you can create. For more information, see Amazon VPC Limits.
NatGatewayMalformed The specified NAT gateway ID is not formed correctly. Ensure that you specify the NAT gateway ID in the form nat-xxxxxxxxxxxxxxxxx.
NatGatewayNotFound The specified NAT gateway does not exist. Ensure that you have indicated the AWS Region in which the NAT gateway is located, if it's not in the default Region.
NetworkAclEntryAlreadyExists The specified rule number already exists in this network ACL.
NetworkAclEntryLimitExceeded You've reached the limit on the number of rules that you can add to the network ACL. For more information, see Amazon VPC Limits.
NetworkAclLimitExceeded You've reached the limit on the number of network ACLs that you can create for the specified VPC. For more information, see Amazon VPC Limits. To request an increase on your network ACL limit, complete the Amazon VPC Limits form.
NetworkInterfaceLimitExceeded You've reached the limit on the number of network interfaces that you can create. For more information, see Amazon VPC Limits.
NetworkInterfaceNotFoundException The specified network interface does not exist.
NetworkInterfaceNotSupportedException The network interface is not supported for Traffic Mirror.
NetworkLoadBalancerNotFoundException The specified Network Load Balancer does not exist.
NlbInUseByTrafficMirrorTargetException The Network Load Balancer is already configured as a Traffic Mirror target.
NonEBSInstance The specified instance does not support Amazon EBS. Restart the instance and try again, to ensure that the code is run on an instance with updated code.
NoSuchVersion The specified API version does not exist.
NotExportable The specified instance cannot be exported. You can only export certain instances. For more information, see Considerations When Using VM Export.
OperationNotPermitted The specified operation is not allowed. This error can occur for a number of reasons; for example, you might be trying to terminate an instance that has termination protection enabled, or trying to detach the primary network interface (eth0) from an instance.
OutstandingVpcPeeringConnectionLimitExceeded You've reached the limit on the number of VPC peering connection requests that you can create for the specified VPC.
PendingSnapshotLimitExceeded You've reached the limit on the number of Amazon EBS snapshots that you can have in the pending state.
PendingVpcPeeringConnectionLimitExceeded You've reached the limit on the number of pending VPC peering connections that you can have.
PlacementGroupLimitExceeded You've reached the limit on the number of placement groups that you can have.
PrivateIpAddressLimitExceeded You've reached the limit on the number of private IP addresses that you can assign to the specified network interface for that type of instance. For more information about the maximum number of private IP addresses per elastic network interface, see Private IP addresses per ENI.
RequestResourceCountExceeded Details in your Spot request exceed the numbers allowed by the Spot service in one of the following ways, depending on the action that generated the error: —If you get this error when you submitted a request for Spot Instances, check the number of Spot Instances specified in your request. The number shouldn't exceed the 3,000 maximum allowed per request. Resend your Spot Instance request and specify a number less than 3,000. If your account's regional Spot request limit is greater than 3,000 instances, you can access these instances by submitting multiple smaller requests.  —If you get this error when you sent Describe Spot Instance requests, check the number of requests for Spot Instance data, the amount of data you requested, and how often you sent the request. The frequency with which you requested the data combined with the amount of data exceeds the levels allowed by the Spot service. Try again and submit fewer large Describe requests over longer intervals.
ReservationCapacityExceeded The targeted Capacity Reservation does not enough available instance capacity to fulfill your request. Either increase the instance capacity for the targeted Capacity Reservation, or target a different Capacity Reservation.
ReservedInstancesCountExceeded You've reached the limit for the number of Reserved Instances.
ReservedInstancesLimitExceeded Your current quota does not allow you to purchase the required number of Reserved Instances.
ReservedInstancesUnavailable The requested Reserved Instances are not available.
Resource.AlreadyAssigned The specified private IP address is already assigned to a resource. Unassign the private IP first, or use a different private IP address.
Resource.AlreadyAssociated The specified resource is already in use. For example, in EC2-VPC, you cannot associate an Elastic IP address with an instance if it's already associated with another instance. You also cannot attach an internet gateway to more than one VPC at a time.
ResourceCountExceeded You have exceeded the number of resources allowed for this request; for example, if you try to launch more instances than AWS allows in a single request. This limit is separate from your individual resource limit. If you get this error, break up your request into smaller requests; for example, if you are launching 15 instances, try launching 5 instances in 3 separate requests.
ResourceCountLimitExceeded You have exceeded a resource limit for creating routes.
ResourceLimitExceeded You have exceeded an Amazon EC2 resource limit. For example, you might have too many snapshot copies in progress.
RouteAlreadyExists A route for the specified CIDR block already exists in this route table.
RouteLimitExceeded You've reached the limit on the number of routes that you can add to a route table.
RouteTableLimitExceeded You've reached the limit on the number of route tables that you can create for the specified VPC. For more information about route table limits, see Amazon VPC Limits.
RulesPerSecurityGroupLimitExceeded You've reached the limit on the number of rules that you can add to a security group. The limit depends on whether you are using EC2-Classic or EC2-VPC. For more information, see Security Group Rules.
ScheduledInstanceLimitExceeded You've reached the limit on the number of Scheduled Instances that you can purchase.
ScheduledInstanceParameterMismatch The launch specification does not match the details for the Scheduled Instance.
ScheduledInstanceSlotNotOpen You can launch a Scheduled Instance only during its scheduled time periods.
ScheduledInstanceSlotUnavailable The requested Scheduled Instance is no longer available during this scheduled time period.
SecurityGroupLimitExceeded You've reached the limit on the number of security groups that you can create, or that you can assign to an instance. The limit depends on whether you are using EC2-Classic or EC2-VPC. For more information, see Creating Your Own Security Groups.
SecurityGroupsPerInstanceLimitExceeded You've reached the limit on the number of security groups that you can assign to an instance. The limit depends on whether you are using EC2-Classic or EC2-VPC. For more information, see Amazon EC2 Security Groups.
SecurityGroupsPerInterfaceLimitExceeded You've reached the limit on the number of security groups you can associate with the specified network interface. For more information, see Amazon VPC Limits.
SignatureDoesNotMatch The request signature that Amazon has does not match the signature that you provided. Check your AWS access keys and signing method.
SnapshotCopyUnsupported.InterRegion Inter-region snapshot copy is not supported for this AWS Region.
SnapshotCreationPerVolumeRateExceeded The rate limit for creating concurrent snapshots of an EBS volume has been exceeded. Wait at least 15 seconds between concurrent volume snapshots.
SnapshotLimitExceeded You've reached the limit on the number of Amazon EBS snapshots that you can create. To request an increase on your snapshot limit, complete the Amazon EC2 EBS Volume Limit Form.
SubnetLimitExceeded You've reached the limit on the number of subnets that you can create for the specified VPC. For more information about subnet limits, see Amazon VPC Limits. To request an increase on your subnet limit, complete the Amazon VPC Limits form.
TagLimitExceeded You've reached the limit on the number of tags that you can assign to the specified resource. For more information, see Tag restrictions.
TargetCapacityLimitExceededException The value for targetCapacity exceeds your limit on the amount of Spot placement target capacity you can explore. Reduce the targetCapacity value, and try again. For more information, see Spot placement score.
TrafficMirrorFilterInUse The Traffic Mirror filter cannot be deleted because a Traffic Mirror session is currently using it.
TrafficMirrorSessionsPerInterfaceLimitExceeded The allowed number of Traffic Mirror sessions for the specified network interface has been exceeded.
TrafficMirrorSessionsPerTargetLimitExceeded The maximum number of Traffic Mirror sessions for the specified Traffic Mirror target has been exceeded.
TrafficMirrorSourcesPerTargetLimitExceeded The maximum number of Traffic Mirror sources for the specified Traffic Mirror target has been exceeded.
TrafficMirrorTargetInUseException The Traffic Mirror target cannot be deleted because a Traffic Mirror session is currently using it.
TrafficMirrorFilterLimitExceeded The maximum number of Traffic Mirror filters has been exceeded.
TrafficMirrorFilterRuleLimitExceeded The maximum number of Traffic Mirror filter rules has been exceeded.
TrafficMirrorSessionLimitExceeded The maximum number of Traffic Mirror sessions has been exceeded.
TrafficMirrorTargetLimitExceeded The maximum number of Traffic Mirror targets has been exceeded.
TrafficMirrorFilterRuleAlreadyExists The Traffic Mirror filter rule already exists.
UnavailableHostRequirements There are no valid Dedicated Hosts available on which you can launch an instance.
UnknownPrincipalType.Unsupported The principal type is not supported. The principal must be an IAM user, IAM role, or the root user for the AWS account.
UnknownVolumeType The specified volume type is unsupported. The supported volume types are gp2, io1, st1, sc1, and standard.
Unsupported The specified request is unsupported. For example, you might be trying to launch an instance in an Availability Zone that currently has constraints on that instance type. The returned message provides details of the unsupported request.
UnsupportedException Capacity Reservations are not supported for this Region.
UnsupportedHibernationConfiguration The instance could not be launched because one or more parameter values do not meet the prerequisites for enabling hibernation. For more information, see Hibernation Prerequisites. Alternatively, the instance could not be hibernated because it is not enabled for hibernation.
UnsupportedHostConfiguration The specified Dedicated Host configuration is unsupported. For more information about supported configurations, see Dedicated Hosts.
UnsupportedInstanceTypeOnHost The instance type is not supported on the Dedicated Host. For more information about supported instance types, see Amazon EC2 Dedicated Hosts Pricing.
UnsupportedTenancy The specified tenancy is unsupported. You can change the tenancy of a VPC to default only.
UpdateLimitExceeded The default credit specification for an instance family can be modified only once in a rolling 5-minute period, and up to four times in a rolling 24-hour period. For more information, see Setting the Default Credit Specification for the Account.
VolumeInUse The specified Amazon EBS volume is attached to an instance. Ensure that the specified volume is in an ‘available’ state.
VolumeIOPSLimit The maximum IOPS limit for the volume has been reached. For more information, see Amazon EBS Volume Types.
VolumeLimitExceeded You've reached the limit on your Amazon EBS volume storage. To request an increase, complete the Amazon EC2 EBS Volume Limit Form.
VolumeModificationSizeLimitExceeded You've reached the limit on your Amazon EBS volume modification storage in this Region. To request an increase, complete the Amazon EC2 EBS Volume Limit Form, or wait for the current volume modifications to complete and then retry your request.
VolumeTypeNotAvailableInZone The specified Availability Zone does not support Provisioned IOPS SSD volumes. Try launching your instance in a different Availability Zone, or don't specify a zone in the request. If you're creating a volume, try specifying a different Availability Zone in the request.
VpcCidrConflict You cannot enable a VPC for ClassicLink if the VPC has routing that conflicts with the EC2-Classic private IP address range of 10/8; for example, if your VPC's route table points to 10.0.0.0/16 for a VPC peering connection. This excludes local routes for VPCs in the 10.0.0.0/16 and 10.1.0.0/16 IP address ranges. For more information, see Routing for Classic Link.
VPCIdNotSpecified You have no default VPC in which to carry out the request. Specify a VPC or subnet ID or, in the case of security groups, specify the ID and not the security group name. To create a new default VPC, contact AWS Support.
VpcEndpointLimitExceeded You've reached the limit on the number of VPC endpoints that you can create in the AWS Region. For more information about VPC limits, see Amazon VPC Limits. To request an increase on your VPC limit, complete the Amazon VPC Limits form.
VpcLimitExceeded You've reached the limit on the number of VPCs that you can create in the AWS Region. For more information about VPC limits, see Amazon VPC Limits. To request an increase on your VPC limit, complete the Amazon VPC Limits form.
VpcPeeringConnectionAlreadyExists A VPC peering connection between the VPCs already exists.
VpcPeeringConnectionsPerVpcLimitExceeded You've reached the limit on the number of VPC peering connections that you can have per VPC. For more information, see Amazon VPC Limits.
VPCResourceNotSpecified The specified resource can be used only in a VPC; for example, T2 instances. Ensure that you have a VPC in your account, and then specify a subnet ID or network interface ID in the request.
VpnConnectionLimitExceeded You've reached the limit on the number of VPN connections that you can create. For more information about limits, see Amazon VPC Limits. To request an increase on your VPN connection limit, complete the Amazon VPC Limits form.
VpnGatewayAttachmentLimitExceeded You've reached the limit on the number of VPCs that can be attached to the specified virtual private gateway.
VpnGatewayLimitExceeded You've reached the limit on the number of virtual private gateways that you can create. For more information about limits, see Amazon VPC Limits. To request an increase on your virtual private gateway limit, complete the Amazon VPC Limits form.
ZonesMismatched The Availability Zone for the instance does not match that of the Dedicated Host.
|}
  ;;

  let enumerate () = generate text_blob
end

module Server_errors = struct
  let text_blob =
    {|
InsufficientAddressCapacity Not enough available addresses to satisfy your minimum request. Reduce the number of addresses you are requesting or wait for additional capacity to become available.
InsufficientCapacity There is not enough capacity to fulfill your import instance request. You can wait for additional capacity to become available.
InsufficientInstanceCapacity There is not enough capacity to fulfill your request. This error can occur if you launch a new instance, restart a stopped instance, create a new Capacity Reservation, or modify an existing Capacity Reservation. Reduce the number of instances in your request, or wait for additional capacity to become available. You can also try launching an instance by selecting different instance types (which you can resize at a later stage). The returned message might also give specific guidance about how to solve the problem.
InsufficientHostCapacity There is not enough capacity to fulfill your Dedicated Host request. Reduce the number of Dedicated Hosts in your request, or wait for additional capacity to become available.
InsufficientReservedInstanceCapacity Not enough available Reserved Instances to satisfy your minimum request. Reduce the number of Reserved Instances in your request or wait for additional capacity to become available.
InsufficientVolumeCapacity There is not enough capacity to fulfill your EBS volume provision request. You can try to provision a different volume type, EBS volume in a different availability zone, or you can wait for additional capacity to become available.
InternalError An internal error has occurred. Retry your request, but if the problem persists, contact us with details by posting a message on the AWS forums.
InternalFailure The request processing has failed because of an unknown error, exception, or failure.
RequestLimitExceeded The maximum request rate permitted by the Amazon EC2 APIs has been exceeded for your account. For best results, use an increasing or variable sleep interval between requests. For more information, see Query API request rate.
ServiceUnavailable The request has failed due to a temporary failure of the server.
Unavailable The server is overloaded and can't handle the request.
|}
  ;;

  let enumerate () = generate text_blob
end

let enumerate_all () =
  Common_client_errors.enumerate ()
  @ Client_errors_for_specific_actions.enumerate ()
  @ Server_errors.enumerate ()
;;
