module Cfg = Awsm_async.Cfg

module Ec2 = struct
  module Values = Awsm_ec2_async.Values
  module Io = Awsm_ec2_async.Io

  let call = Awsm_async.Http.Io.call ~service:Values.service
end

let default_region = "us-east-2"
let instance_type = Ec2.Values.InstanceType.T4g_nano
let architecture = "arm64"

let dispatch_exn ~name ~sexp_of_error ~f =
  match%bind f () with
  | Ok v -> return v
  | Error (`Transport err) ->
    failwithf "%s: %s" name (Awsm.Http.Io.Error.sexp_of_call err |> Sexp.to_string_hum) ()
  | Error (`AWS aws) ->
    failwithf "%s: %s" name (aws |> sexp_of_error |> Sexp.to_string_hum) ()
;;

let call = ref None

let ec2 name f r =
  let call = Option.value_exn ~message:"need to intialize call first" !call in
  dispatch_exn ~name ~sexp_of_error:Ec2.Values.Ec2_error.sexp_of_t ~f:(fun () -> f call r)
;;

let describe_instances_no_terminated () =
  ec2
    "describe-instances"
    Ec2.Io.describe_instances
    (Ec2.Values.DescribeInstancesRequest.make ())
  >>| fun result ->
  result.reservations
  |> Option.value_exn ~here:[%here]
  |> List.concat_map ~f:(fun reservation ->
       reservation.Ec2.Values.Reservation.instances
       |> Option.value ~default:[]
       |> List.filter ~f:(fun instance ->
            match instance.Ec2.Values.Instance.state with
            | None -> true
            | Some { Ec2.Values.InstanceState.name; _ } -> (
              match name with
              | Some Ec2.Values.InstanceStateName.Terminated -> false
              | None | Some _ -> true)))
;;

let describe_instance_attribute ~instance_id ~attribute =
  ec2
    "describe-instance-attribute"
    Ec2.Io.describe_instance_attribute
    (Ec2.Values.DescribeInstanceAttributeRequest.make
       ~instanceId:instance_id
       ~attribute:(Ec2.Values.InstanceAttributeName.of_string attribute)
       ())
;;

let modify_instance_attribute_userdata ~instance_id ~user_data =
  let userData =
    let value = Base64.encode_exn user_data in
    printf "base64 encoded user_data: %s\n" value;
    Ec2.Values.BlobAttributeValue.make ~value ()
  in
  ec2
    "modify-instance-attribute"
    Ec2.Io.modify_instance_attribute
    (Ec2.Values.ModifyInstanceAttributeRequest.make ~instanceId:instance_id ~userData ())
;;

let modify_instance_metadata_options ~instance_id ~http_endpoint:httpEndpoint =
  ec2
    "modify-instance-metadata-options"
    Ec2.Io.modify_instance_metadata_options
    (Ec2.Values.ModifyInstanceMetadataOptionsRequest.make
       ~instanceId:instance_id
       ~httpEndpoint
       ())
;;

let describe_instances_status ~instance_id =
  ec2
    "describe-instances-status"
    Ec2.Io.describe_instance_status
    (Ec2.Values.DescribeInstanceStatusRequest.make
       ~includeAllInstances:true
       ~instanceIds:[ instance_id ]
       ())
  >>| fun result ->
  match result.instanceStatuses with
  | None | Some [] -> None
  | Some [ status ] -> (
    match status.Ec2.Values.InstanceStatus.instanceState with
    | None -> failwithf "no instance status?" ()
    | Some { Ec2.Values.InstanceState.name; _ } -> name)
  | Some lst -> failwithf "too many instances, expected one got %d" (List.length lst) ()
;;

let describe_instance_type_offerings ~instance_type () =
  ec2
    "describe-instance-type-offerings"
    Ec2.Io.describe_instance_type_offerings
    (Ec2.Values.DescribeInstanceTypeOfferingsRequest.make
       ~filters:
         [ { Ec2.Values.Filter.name = Some "instance-type"
           ; values = Some [ instance_type ]
           }
         ]
       ())
;;

let describe_addresses () =
  ec2
    "describe-addresses"
    Ec2.Io.describe_addresses
    (Ec2.Values.DescribeAddressesRequest.make ())
  >>| fun { Ec2.Values.DescribeAddressesResult.addresses; _ } ->
  Option.value_exn ~here:[%here] addresses
;;

let describe_images ?filters ?owners () =
  ec2
    "describe-images"
    Ec2.Io.describe_images
    (Ec2.Values.DescribeImagesRequest.make ?filters ?owners ())
  >>| fun { Ec2.Values.DescribeImagesResult.images; _ } ->
  Option.value_exn ~here:[%here] images
;;

let describe_instance_types () =
  let rec loop ?nextToken acc =
    let%bind { instanceTypes; nextToken; _ } =
      ec2
        "describe-instances-types"
        Ec2.Io.describe_instance_types
        (Ec2.Values.DescribeInstanceTypesRequest.make ?nextToken ())
    in
    let results =
      Option.value_exn ~here:[%here] instanceTypes
      |> List.map ~f:(fun { instanceType; _ } ->
           instanceType
           |> Option.value_exn ~here:[%here]
           |> Ec2.Values.InstanceType.sexp_of_t
           |> Sexp.to_string)
    in
    match nextToken with
    | None -> return acc
    | Some nextToken ->
      let%bind () = Clock.after (sec 5.0) in
      loop ~nextToken (results @ acc)
  in
  loop [] >>| List.sort ~compare:String.compare
;;

let describe_network_interfaces () =
  ec2
    "describe-network-interfaces"
    Ec2.Io.describe_network_interfaces
    (Ec2.Values.DescribeNetworkInterfacesRequest.make ())
  >>| fun nis -> Option.value_exn ~here:[%here] nis.networkInterfaces
;;

let describe_network_interface_attribute ~network_interface_id:networkInterfaceId () =
  ec2
    "describe-network-interface-attribute"
    Ec2.Io.describe_network_interface_attribute
    (Ec2.Values.DescribeNetworkInterfaceAttributeRequest.make
       ~networkInterfaceId
       ~attribute:Ec2.Values.NetworkInterfaceAttribute.Description
       ())
;;

let modify_network_interface_attribute
  ~network_interface_id:networkInterfaceId
  ~description
  ()
  =
  ec2
    "modify-network-interface-attribute"
    Ec2.Io.modify_network_interface_attribute
    (Ec2.Values.ModifyNetworkInterfaceAttributeRequest.make
       ~networkInterfaceId
       ~description:(Ec2.Values.AttributeValue.make ~value:description ())
       ())
;;

let assign_private_ip_address ~network_interface_id:networkInterfaceId ~ip_address =
  ec2
    "assign-private-ip-address"
    Ec2.Io.assign_private_ip_addresses
    (Ec2.Values.AssignPrivateIpAddressesRequest.make
       ~networkInterfaceId
       ~privateIpAddresses:[ ip_address ]
       ())
;;

let describe_security_groups () =
  ec2
    "describe-security-groups"
    Ec2.Io.describe_security_groups
    (Ec2.Values.DescribeSecurityGroupsRequest.make ())
  >>| fun sg -> Option.value_exn ~here:[%here] sg.securityGroups
;;

let describe_snapshots () =
  ec2
    "describe-snapshots"
    Ec2.Io.describe_snapshots
    (Ec2.Values.DescribeSnapshotsRequest.make ~ownerIds:[ "self" ] ())
  >>| fun sns -> Option.value_exn ~here:[%here] sns.snapshots
;;

let describe_subnets () =
  ec2
    "describe-subnets"
    Ec2.Io.describe_subnets
    (Ec2.Values.DescribeSubnetsRequest.make ())
  >>| fun sns ->
  assert (Option.is_none sns.nextToken);
  Option.value_exn ~here:[%here] sns.subnets
;;

let describe_volumes () =
  ec2
    "describe-volumes"
    Ec2.Io.describe_volumes
    (Ec2.Values.DescribeVolumesRequest.make ())
  >>| fun v ->
  assert (Option.is_none v.nextToken);
  Option.value_exn ~here:[%here] v.volumes
;;

let describe_volumes_modifications () =
  ec2
    "describe-volumes"
    Ec2.Io.describe_volumes_modifications
    (Ec2.Values.DescribeVolumesModificationsRequest.make ())
  >>| fun v ->
  assert (Option.is_none v.nextToken);
  Option.value_exn ~here:[%here] v.volumesModifications
;;

let describe_vpcs () =
  ec2 "describe-vpcs" Ec2.Io.describe_vpcs (Ec2.Values.DescribeVpcsRequest.make ())
  >>| fun v ->
  assert (Option.is_none v.nextToken);
  Option.value_exn ~here:[%here] v.vpcs
;;

let require_empty_list description d =
  match%bind d with
  | [] -> return ()
  | lst ->
    failwithf
      "expected to see zero %s in this zone. found %d"
      description
      (List.length lst)
      ()
;;

let require_singleton_list description = function
  | None | Some [] -> failwithf "%s: got zero, expected one" description ()
  | Some [ x ] -> x
  | Some lst -> failwithf "%s: expected one, got %d" description (List.length lst) ()
;;

let retry_forever ?(interval = sec 5.0) ~description f =
  printf
    !"waiting for %s (retry every %s)...\n"
    description
    (Time.Span.to_string_hum interval);
  let rec loop () =
    match%bind f () with
    | Some result -> return result
    | None ->
      let%bind () = Clock.after interval in
      loop ()
  in
  loop ()
;;

let sexp_lower s = s |> Sexp.to_string |> String.lowercase

let wait_for_instance_state ~instance_id ~state_name ~state =
  retry_forever
    ~description:(sprintf "instance %s to enter state '%s'" state_name instance_id)
    (fun () ->
    match%map describe_instances_status ~instance_id with
    | None ->
      printf ". <no instance status yet>\n";
      None
    | Some name ->
      printf ". %s\n" (name |> Ec2.Values.InstanceStateName.sexp_of_t |> sexp_lower);
      if Stdlib.( = ) name state then Some () else None)
;;

let wait_for_instance_running_state =
  wait_for_instance_state
    ~state_name:"running"
    ~state:Ec2.Values.InstanceStateName.Running
;;

let wait_for_instance_stopped_state =
  wait_for_instance_state
    ~state_name:"stopped"
    ~state:Ec2.Values.InstanceStateName.Stopped
;;

let wait_for_volume_available ~volume_id =
  retry_forever
    ~description:(sprintf "volume %s to enter state 'available'" volume_id)
    (fun () ->
    let%map volumes = describe_volumes () in
    let volume =
      List.find volumes ~f:(function
        | { volumeId = None; _ } -> false
        | { volumeId = Some volumeId; _ } -> String.( = ) volume_id volumeId)
      |> Option.value_exn ~message:(sprintf "volume %s not found?!" volume_id)
    in
    match
      volume.Ec2.Values.Volume.state |> Option.value_exn ~message:"no volume.state?!"
    with
    | Ec2.Values.VolumeState.Available -> Some volume
    | other_state ->
      printf !". %s\n" (other_state |> Ec2.Values.VolumeState.sexp_of_t |> sexp_lower);
      None)
;;

let wait_for_volume_modification_completed ~volume_id =
  retry_forever
    ~description:
      (sprintf
         "waiting for volume %s to enter state 'completed' or 'optimizing'"
         volume_id)
    (fun () ->
    let%map modifications = describe_volumes_modifications () in
    let mod_ =
      List.find modifications ~f:(function
        | { volumeId = None; _ } -> false
        | { volumeId = Some volumeId; _ } -> String.( = ) volume_id volumeId)
      |> Option.value_exn ~message:(sprintf "volume %s not found?!" volume_id)
    in
    let state =
      mod_.Ec2.Values.VolumeModification.modificationState
      |> Option.value_exn ~message:"no modificationState?!"
    in
    printf !". %s\n" (state |> Ec2.Values.VolumeModificationState.sexp_of_t |> sexp_lower);
    match state with
    | Ec2.Values.VolumeModificationState.(Completed | Optimizing) -> Some mod_
    | _ -> None)
;;

let wait_for_network_interface_available ~network_interface_id =
  retry_forever
    ~description:
      (sprintf
         "waiting for network interface %s to enter state 'available'"
         network_interface_id)
    (fun () ->
    let%map interfaces = describe_network_interfaces () in
    let interface =
      List.find interfaces ~f:(function
        | { networkInterfaceId = None; _ } -> false
        | { networkInterfaceId = Some networkInterfaceId; _ } ->
          String.( = ) network_interface_id networkInterfaceId)
      |> Option.value_exn
           ~message:(sprintf "network_interface %s not found?!" network_interface_id)
    in
    match
      interface.Ec2.Values.NetworkInterface.status
      |> Option.value_exn ~message:"no networkinterface.status?!"
    with
    | Ec2.Values.NetworkInterfaceStatus.Available -> Some interface
    | other_state ->
      printf
        !". %s\n"
        (other_state |> Ec2.Values.NetworkInterfaceStatus.sexp_of_t |> sexp_lower);
      None)
;;

let print_azs () =
  printf "describe-availability-zones...\n";
  ec2
    "describe-availability-zones"
    Ec2.Io.describe_availability_zones
    (Ec2.Values.DescribeAvailabilityZonesRequest.make ())
  >>| fun { availabilityZones; _ } ->
  availabilityZones
  |> Option.value_exn ~here:[%here]
  |> List.iter ~f:(fun az ->
       Option.value_exn ~here:[%here] az.zoneName |> printf ". %s\n")
;;

let _print_instance_types () =
  printf "describe-instance-types (can take a bit)...\n";
  describe_instance_types () >>| fun ss -> List.iter ss ~f:(printf ". %s\n")
;;

let print_instance_type_offerings () =
  printf "describe-instance-type-offerings instance-type=t4g.nano...\n";
  describe_instance_type_offerings ~instance_type:"t4g.nano" ()
  >>| fun tos ->
  tos.instanceTypeOfferings
  |> Option.value_exn ~here:[%here] ~message:"no instanceTypeOfferings?!"
  |> List.iter ~f:(fun o ->
       o
       |> Ec2.Values.InstanceTypeOffering.sexp_of_t
       |> Sexp.to_string_hum
       |> print_endline)
;;

let _print_volumes () =
  printf "describe-volumes...\n";
  describe_volumes ()
  >>| List.iter ~f:(fun { Ec2.Values.Volume.volumeId; _ } ->
        Option.value_exn ~here:[%here] volumeId |> printf ". %s\n")
;;

let print_security_groups () =
  printf "describe-security-groups...\n";
  describe_security_groups ()
  >>| fun sgs ->
  List.iter sgs ~f:(fun { groupName; _ } ->
    groupName |> Option.value_exn ~here:[%here] |> printf ". %s\n")
;;

(*
let print_snapshots () =
  printf "describe-snapshots (owner=self)...\n";
  describe_snapshots ()
  >>| fun snps ->
  List.iter snps ~f:(fun snp ->
    snp.snapshotId |> Option.value_exn ~here:[%here] |> printf ". %s\n")
;;
*)

let print_subnets () =
  printf "describe-subnets...\n";
  describe_subnets ()
  >>| fun subs ->
  List.iter subs ~f:(fun subnet ->
    subnet.subnetId |> Option.value_exn ~here:[%here] |> printf ". %s\n")
;;

let print_vpcs () =
  printf "describe-vpcs...\n";
  describe_vpcs ()
  >>| fun vpcs ->
  List.iter vpcs ~f:(fun { vpcId; _ } ->
    vpcId |> Option.value_exn ~here:[%here] |> printf ". %s\n")
;;

let check_for_clean_slate () =
  printf "checking to make sure we're starting with a clean slate...\n";
  let empty = require_empty_list in
  printf ". check ec2 addresses...\n";
  let%bind () = empty "EC2 addresses" (describe_addresses ()) in
  printf ". check ec2 images (owner=self)...\n";
  let%bind () = empty "EC2 images" (describe_images ~owners:[ "self" ] ()) in
  printf ". check ec2 instances...\n";
  let%bind () = empty "EC2 instances" (describe_instances_no_terminated ()) in
  printf ". check ec2 snapshots...\n";
  let%bind () = empty "EC2 snapshots" (describe_snapshots ()) in
  printf ". check ec2 volumes...\n";
  let%bind () = empty "EC2 volumes" (describe_volumes ()) in
  printf "done checking to see if we're starting with a clean slate\n";
  return ()
;;

let test_address_operations ~instance_id =
  printf "allocate-address...\n";
  let%bind address_alloc =
    ec2
      "allocate_address"
      Ec2.Io.allocate_address
      (Ec2.Values.AllocateAddressRequest.make ())
  in
  let public_ip = address_alloc.publicIp |> Option.value_exn ~message:"no publicIp" in
  printf "associate-address: %s to %s...\n" public_ip instance_id;
  let%bind associate_addr =
    ec2
      "associate-address"
      Ec2.Io.associate_address
      (Ec2.Values.AssociateAddressRequest.make
         ~publicIp:public_ip
         ~instanceId:instance_id
         ())
  in
  let association_id =
    associate_addr.associationId |> Option.value_exn ~message:"no associationId?!"
  in
  printf "public IP address associated; association-id: %s\n" association_id;
  printf "disassociate-address %s on (%s, %s)...\n" association_id instance_id public_ip;
  let%bind () =
    ec2
      "disassociate-address"
      Ec2.Io.disassociate_address
      (Ec2.Values.DisassociateAddressRequest.make ~associationId:association_id ())
  in
  printf "release-address %s...\n" public_ip;
  let%bind () =
    ec2
      "release-address"
      Ec2.Io.release_address
      (Ec2.Values.ReleaseAddressRequest.make
         ~allocationId:
           (Option.value_exn address_alloc.allocationId ~message:"no allocationId?!")
         ())
  in
  return ()
;;

let test_network_interface_operations ~instance_id ~availability_zone =
  printf "searching for a subnet...\n";
  let%bind subnets = describe_subnets () in
  (* Just pick the first subnet we find in the same AZ. *)
  let subnet_id, cidr_block =
    match
      List.find subnets ~f:(fun subnet ->
        let az =
          subnet.Ec2.Values.Subnet.availabilityZone
          |> Option.value_exn ~message:"no availabilityZone?!"
        in
        String.( = ) az availability_zone)
    with
    | None -> failwithf "couldn't find a subnet to use" ()
    | Some subnet ->
      let subnet_id = subnet.subnetId |> Option.value_exn ~message:"no subnetId?!" in
      let cidr_block = subnet.cidrBlock |> Option.value_exn ~message:"no cidrBlock?!" in
      subnet_id, cidr_block
  in
  printf "found subnet_id=%s with cidr_block=%s\n" subnet_id cidr_block;
  printf "create-network-interface...\n";
  let%bind interface_result =
    ec2
      "create-network-interface"
      Ec2.Io.create_network_interface
      (Ec2.Values.CreateNetworkInterfaceRequest.make ~subnetId:subnet_id ())
  in
  let network_interface_id =
    let network_interface =
      interface_result.networkInterface
      |> Option.value_exn ~message:"no networkInterface?!"
    in
    network_interface.networkInterfaceId
    |> Option.value_exn ~message:"no networkInterfaceId?!"
  in
  printf "network interface created: %s\n" network_interface_id;
  let%bind _ = wait_for_network_interface_available ~network_interface_id in
  let%bind () =
    let description = "test description" in
    printf
      "modify-network-interface-attribute network_interface_id=%s, description=%s...\n"
      network_interface_id
      description;
    let%bind () =
      modify_network_interface_attribute ~network_interface_id ~description ()
    in
    printf
      "describe network interface attribute network_interface_id=%s...\n"
      network_interface_id;
    let%bind attribute = describe_network_interface_attribute ~network_interface_id () in
    let () =
      attribute
      |> Ec2.Values.DescribeNetworkInterfaceAttributeResult.sexp_of_t
      |> Sexp.to_string_hum
      |> print_endline
    in
    return ()
  in
  printf "attach-network-interface %s to %s...\n" network_interface_id instance_id;
  let%bind attach_result =
    ec2
      "attach-network-interface"
      Ec2.Io.attach_network_interface
      (Ec2.Values.AttachNetworkInterfaceRequest.make
         ~instanceId:instance_id
         ~networkInterfaceId:network_interface_id
         ~deviceIndex:1
         ())
  in
  let attachment_id =
    attach_result.attachmentId |> Option.value_exn ~message:"no attachmentId?!"
  in
  printf "attached to %s: %s\n" instance_id attachment_id;
  printf
    "detach-network-interface %s from %s (%s)...\n"
    network_interface_id
    instance_id
    attachment_id;
  let%bind () =
    ec2
      "detach-network-interface"
      Ec2.Io.detach_network_interface
      (Ec2.Values.DetachNetworkInterfaceRequest.make
         ~force:true
         ~attachmentId:attachment_id
         ())
  in
  let%bind () =
    let ip_address =
      (* Some IP addresses in the subnet are reserved. For a CIDR like 10.0.0.0/24, we have:

          10.0.0.0 (network address)
          10.0.0.1 (reserved by AWS for the VPC router)
          10.0.0.2 (reserved by AWS for DNS resolution)
          10.0.0.3 (reserved by AWS for future use)
          10.0.0.255 (broadcast address)

         So whatever IP base a.b.c.d we get, lets try a.b.c.(d+4).  *)
      let ip_base, _ = String.lsplit2_exn ~on:'/' cidr_block in
      match String.split ~on:'.' ip_base with
      | [ a; b; c; d ] -> sprintf "%s.%s.%s.%d" a b c (d |> Int.of_string |> Int.( + ) 4)
      | _ -> failwithf "unknown ip address format for '%s', expected a.b.c.d" ip_base ()
    in
    printf
      "assign-private-ip-address to network_interface_id=%s, ip_address=%s...\n"
      network_interface_id
      ip_address;
    let%bind assign_result =
      assign_private_ip_address ~network_interface_id ~ip_address
    in
    printf
      "assigned private ip addresses: %s\n"
      (assign_result.assignedPrivateIpAddresses
      |> Option.value_exn ~message:"no assignedPrivateIpAddresses?!"
      |> List.map ~f:(function
           | { Ec2.Values.AssignedPrivateIpAddress.privateIpAddress; _ } ->
           privateIpAddress |> Option.value_exn ~message:"no privateIpAddress?!")
      |> String.concat ~sep:",");
    return ()
  in
  let%bind _ = wait_for_network_interface_available ~network_interface_id in
  printf "delete-network-interface %s...\n" network_interface_id;
  let%bind () =
    ec2
      "delete-network-interface"
      Ec2.Io.delete_network_interface
      (Ec2.Values.DeleteNetworkInterfaceRequest.make
         ~networkInterfaceId:network_interface_id
         ())
  in
  return ()
;;

let test_volume_operations ~instance_id ~availability_zone =
  printf "create-volume...\n";
  let%bind volume =
    ec2
      "create-volume"
      Ec2.Io.create_volume
      (Ec2.Values.CreateVolumeRequest.make ~size:1 ~availabilityZone:availability_zone ())
  in
  let volume_id =
    volume.Ec2.Values.Volume.volumeId |> Option.value_exn ~message:"no volumeId?!"
  in
  printf "created volume with volume-id: %s\n" volume_id;
  let%bind _ = wait_for_volume_available ~volume_id in
  printf "modify-volume %s 1GB -> 2GB...\n" volume_id;
  let%bind modify_result =
    ec2
      "modify-volume"
      Ec2.Io.modify_volume
      (Ec2.Values.ModifyVolumeRequest.make ~size:2 ~volumeId:volume_id ())
  in
  let _volume_modfiication =
    modify_result.volumeModification
    |> Option.value_exn ~message:"no volumeModification?!"
  in
  let%bind _ = wait_for_volume_modification_completed ~volume_id in
  printf "attach-volume %s to %s...\n" volume_id instance_id;
  let%bind _volume_attachment =
    ec2
      "attach-volume"
      Ec2.Io.attach_volume
      (Ec2.Values.AttachVolumeRequest.make
         ~volumeId:volume_id
         ~instanceId:instance_id
         ~device:"/dev/sdh"
         ())
  in
  printf "detach-volume %s from %s...\n" volume_id instance_id;
  let%bind _volume_detachment =
    ec2
      "detach-volume"
      Ec2.Io.detach_volume
      (Ec2.Values.DetachVolumeRequest.make ~volumeId:volume_id ())
  in
  printf "detach initiated. wait for it to become available before delete...\n";
  let%bind _ = wait_for_volume_available ~volume_id in
  printf "delete-volume %s...\n" volume_id;
  let%bind () =
    ec2
      "delete-volume"
      Ec2.Io.delete_volume
      (Ec2.Values.DeleteVolumeRequest.make ~volumeId:volume_id ())
  in
  return ()
;;

(* AMI IDs vary by region so to be robust to being run in different regions we just
   try to find the latest Amazon Linux AMI with the expected architecture. *)
let find_ami () =
  let filters =
    List.map
      ~f:(fun (k, v) -> { Ec2.Values.Filter.name = Some k; values = Some [ v ] })
      [ "architecture", architecture
      ; "owner-alias", "amazon"
      ; "is-public", "true"
      ; "virtualization-type", "hvm"
      ]
  in
  describe_images ~filters ()
  >>| fun images ->
  List.filter images ~f:(fun (image : Ec2.Values.Image.t) ->
    (* don't pick complicated AMIs *)
    let description = image.description |> Option.value ~default:"" |> String.lowercase in
    String.is_substring description ~substring:"linux"
    && (not (String.is_substring description ~substring:"docker"))
    && (not (String.is_substring description ~substring:"kuber"))
    &&
    match image.platform with
    | Some Ec2.Values.PlatformValues.Windows -> false
    | _ -> true)
  |> List.sort ~compare:(fun a b ->
       match a.creationDate, b.creationDate with
       | None, _ | _, None -> failwithf "find_ami: AMIs have no creation date!?" ()
       | Some ad, Some bd -> String.compare bd ad)
  |> List.hd_exn
  |> fun (ami : Ec2.Values.Image.t) ->
  ami.imageId |> Option.value_exn ~message:"no imageId?!"
;;

let create_tag ~resource_id ~key ~value =
  ec2
    "create-tag"
    Ec2.Io.create_tags
    (Ec2.Values.CreateTagsRequest.make
       ~resources:[ resource_id ]
       ~tags:[ { Ec2.Values.Tag.key = Some key; value = Some value } ]
       ())
;;

let delete_tag ~resource_id ~key ~value =
  ec2
    "delete-tag"
    Ec2.Io.delete_tags
    (Ec2.Values.DeleteTagsRequest.make
       ~resources:[ resource_id ]
       ~tags:[ { Ec2.Values.Tag.key = Some key; value = Some value } ]
       ())
;;

let create_and_delete_tag resource_id =
  let key = "foo"
  and value = "bar" in
  printf "create-tag key=%s, value=%s on instance %s...\n" key value resource_id;
  let%bind () = create_tag ~resource_id ~key ~value in
  printf "done. delete-tag...\n";
  let%bind () = delete_tag ~resource_id ~key ~value in
  printf "done\n";
  return ()
;;

let modify_and_describe_instance_attribute instance_id =
  printf "modify-instance-attribute instance_id=%s attribute=userData...\n" instance_id;
  let%bind () =
    modify_instance_attribute_userdata
      ~instance_id
      ~user_data:
        "#!/bin/bash\n\
         sudo yum update -y\n\
         sudo yum install httpd -y\n\
         sudo service httpd start\n"
  in
  printf "describe-instance-attribute instance_id=%s attribute=userData...\n" instance_id;
  let%bind attribute = describe_instance_attribute ~instance_id ~attribute:"userData" in
  printf
    "instance-attribute: %s\n"
    (attribute |> Ec2.Values.InstanceAttribute.sexp_of_t |> Sexp.to_string_hum);
  return ()
;;

let run_all_tests ?image_id ~region () =
  let%bind () = check_for_clean_slate () in
  let%bind () = print_azs () in
  (*
  let%bind () = print_instance_types () in
  *)
  let%bind () = print_instance_type_offerings () in
  let%bind () = print_security_groups () in
  let%bind () = print_subnets () in
  let%bind () = print_vpcs () in
  printf !"finding a suitable AMI in %{Awsm.Region}...\n" region;
  let%bind ami_id =
    match image_id with
    | Some id -> return id
    | None -> find_ami ()
  in
  printf "found an AMI id: %s\n" ami_id;
  printf "creating EC2 instance...\n";
  let%bind run_result =
    ec2
      "run-instance"
      Ec2.Io.run_instances
      (Ec2.Values.RunInstancesRequest.make
         ~instanceType:instance_type
         ~imageId:ami_id
         ~minCount:1
         ~maxCount:1
         ())
  in
  let instance =
    require_singleton_list
      "run-instances result"
      run_result.Ec2.Values.Reservation.instances
  in
  let instance_id = instance.instanceId |> Option.value_exn ~message:"no instanceId?!" in
  printf "EC2 run initiated: id=%s\n" instance_id;
  let%bind () = create_and_delete_tag instance_id in
  let%bind () = wait_for_instance_running_state ~instance_id in
  let availability_zone =
    (instance.Ec2.Values.Instance.placement |> Option.value_exn ~message:"no placement?!")
      .availabilityZone
    |> Option.value_exn ~message:"no availabilityZone?!"
  in
  let%bind () = test_address_operations ~instance_id in
  let%bind () = test_network_interface_operations ~instance_id ~availability_zone in
  let%bind () = test_volume_operations ~instance_id ~availability_zone in
  printf "reboot-instance...\n";
  let%bind () =
    ec2
      "reboot-instances"
      Ec2.Io.reboot_instances
      (Ec2.Values.RebootInstancesRequest.make ~instanceIds:[ instance_id ] ())
  in
  let%bind _ = wait_for_instance_running_state ~instance_id in
  printf "stop-instances %s...\n" instance_id;
  let%bind stop_result =
    ec2
      "stop-instances"
      Ec2.Io.stop_instances
      (Ec2.Values.StopInstancesRequest.make ~instanceIds:[ instance_id ] ())
  in
  let _ = require_singleton_list "stop-instances result" stop_result.stoppingInstances in
  let%bind () = wait_for_instance_stopped_state ~instance_id in
  let%bind () = modify_and_describe_instance_attribute instance_id in
  let%bind () =
    printf
      "modify-instance-metadata-options instance_id=%s, http_endpoint=true...\n"
      instance_id;
    let%bind meta_modify_result =
      modify_instance_metadata_options
        ~instance_id
        ~http_endpoint:Ec2.Values.InstanceMetadataEndpointState.Disabled
    in
    let () =
      meta_modify_result
      |> Ec2.Values.ModifyInstanceMetadataOptionsResult.sexp_of_t
      |> Sexp.to_string_hum
      |> printf "modify-instance-metadata-options result: %s\n"
    in
    return ()
  in
  printf "terminate-instance %s...\n" instance_id;
  let%bind terminate_result =
    ec2
      "terminate-instances"
      Ec2.Io.terminate_instances
      (Ec2.Values.TerminateInstancesRequest.make ~instanceIds:[ instance_id ] ())
  in
  let _ =
    require_singleton_list
      "terminate-instances result"
      terminate_result.terminatingInstances
  in
  printf "test-suite done!\n";
  return ()
;;

let main ?profile ?image_id ~region () =
  let region = Awsm.Region.of_string region in
  let%bind cfg = Cfg.get_exn ?profile ~region () in
  call := Some (Ec2.call ~cfg);
  run_all_tests ?image_id ~region ()
;;

let () =
  let cmd =
    Command.async
      ~summary:"Test script: we suggest selecting a region you have nothing in"
      (let open Command.Let_syntax in
      let%map_open profile = flag "-profile" (optional string) ~doc:"NAME profile name"
      and image_id =
        flag
          "-image-id"
          (optional string)
          ~doc:"AMI-ID override AMI to use (default: search for Amazon Linux)"
      and region =
        flag
          "-region"
          (optional_with_default "us-east-2" string)
          ~doc:(sprintf "REGION region to run tests in (default: %s)" default_region)
      in
      fun () -> main ?profile ?image_id ~region ())
  in
  Command_unix.run cmd
;;
