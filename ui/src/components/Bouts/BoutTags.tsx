import {
  Autocomplete,
  Avatar,
  Box,
  Button,
  Chip,
  createFilterOptions,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  Divider,
  Popover,
  TextField,
  Typography,
} from "@mui/material";
import _ from "lodash";
import { useState } from "react";

import {
  Bout,
  Tag,
  useBoutTagsQuery,
  useCreateBoutTagMutation,
  useDeleteBoutTagMutation,
  useGetCurrentUserQuery,
  useTagsQuery,
} from "@/graphql/generated";

type TagOption = Pick<Tag, "name" | "description"> & { inputValue?: string };

const filter = createFilterOptions<TagOption>();

// Autocomplete implementation comes from 'free solo dialog' example:
// https://mui.com/material-ui/react-autocomplete/#creatable
export function BoutTags({ bout }: { bout: Pick<Bout, "id"> }) {
  const { moderator, username } = useGetCurrentUserQuery().data
    ?.currentUser ?? {
    moderator: false,
    username: null,
  };
  const tagsQuery = useTagsQuery();
  const tags: TagOption[] = tagsQuery.data?.tags?.results ?? [];
  const boutTagsQuery = useBoutTagsQuery({ boutId: bout.id });
  const boutTags = boutTagsQuery.data?.boutTags?.results ?? [];

  const [tagAnchorEls, setTagAnchorEls] = useState<
    Record<string, HTMLDivElement | null>
  >({});

  const createBoutTag = useCreateBoutTagMutation({
    onSuccess: () => {
      boutTagsQuery.refetch();
      tagsQuery.refetch();
    },
  });

  const groupedBoutTags = _.groupBy(boutTags, (boutTag) => boutTag?.tag?.slug);

  const deleteBoutTag = useDeleteBoutTagMutation({
    onSuccess: () => {
      boutTagsQuery.refetch();
    },
  });

  const [value, setValue] = useState<TagOption | null>(null);
  const [open, toggleOpen] = useState(false);

  const [dialogValue, setDialogValue] = useState({
    name: "",
    description: "",
  });

  const submitTag = (name: string) => {
    createBoutTag.mutate({
      boutId: bout.id,
      tagName: name,
    });
    setValue({ name: "", description: "" });
    setDialogValue({ name: "", description: "" });
  };

  const handleSubmit = (event: React.FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    createBoutTag.mutate({
      boutId: bout.id,
      tagName: dialogValue.name,
      tagDescription: dialogValue.description,
    });
    handleClose();
  };

  const handleClose = () => {
    setDialogValue({
      name: "",
      description: "",
    });
    toggleOpen(false);
  };
  return (
    <>
      <Box>
        {moderator && (
          <>
            <Box mb={4} display="flex" gap={2}>
              <Autocomplete
                autoComplete={true}
                value={value}
                onChange={(event, newValue) => {
                  console.log("change?", newValue);
                  if (typeof newValue === "string") {
                    // timeout to avoid instant validation of the dialog's form.
                    setTimeout(() => {
                      toggleOpen(true);
                      setDialogValue({
                        name: newValue,
                        description: "",
                      });
                    });
                  } else if (newValue && newValue.inputValue) {
                    toggleOpen(true);
                    setDialogValue({
                      name: newValue.inputValue,
                      description: "",
                    });
                  } else if (newValue?.name) {
                    submitTag(newValue.name);
                  }
                }}
                filterOptions={(options, params) => {
                  const filtered = filter(options, params);

                  if (params.inputValue !== "") {
                    filtered.push({
                      inputValue: params.inputValue,
                      name: `Add "${params.inputValue}"`,
                    });
                  }

                  return filtered;
                }}
                options={tags}
                getOptionLabel={(option) => {
                  // for example value selected with enter, right from the input
                  if (typeof option === "string") {
                    return option;
                  }
                  if (option.inputValue) {
                    return option.inputValue;
                  }
                  return option.name;
                }}
                selectOnFocus
                clearOnBlur
                handleHomeEndKeys
                renderOption={(props, option) => {
                  const { key, ...optionProps } = props;
                  return (
                    <li key={key} {...optionProps}>
                      {option.name}
                    </li>
                  );
                }}
                sx={{ width: 300 }}
                freeSolo
                renderInput={(params) => (
                  <TextField
                    {...params}
                    label="Add bout tag"
                    variant="standard"
                  />
                )}
              />
              <Dialog open={open} onClose={handleClose} maxWidth="md">
                <form onSubmit={handleSubmit}>
                  <DialogTitle>Add a new tag</DialogTitle>
                  <DialogContent
                    sx={{ width: (theme) => theme.breakpoints.values.sm }}
                  >
                    <Box display="flex" flexDirection={"column"} gap={3}>
                      <TextField
                        autoFocus
                        margin="dense"
                        value={dialogValue.name}
                        onChange={(event) =>
                          setDialogValue({
                            ...dialogValue,
                            name: event.target.value,
                          })
                        }
                        label="Tag name"
                        type="text"
                        variant="standard"
                      />
                      <TextField
                        variant="standard"
                        type="text"
                        label="Tag description"
                        multiline
                        value={dialogValue.description}
                        onChange={(event) =>
                          setDialogValue({
                            ...dialogValue,
                            description: event.target.value,
                          })
                        }
                      />
                    </Box>
                  </DialogContent>
                  <DialogActions>
                    <Button onClick={handleClose}>Cancel</Button>
                    <Button type="submit">Add</Button>
                  </DialogActions>
                </form>
              </Dialog>
            </Box>
          </>
        )}

        <Box display="flex" gap={1}>
          {Object.entries(groupedBoutTags).map(([tagSlug, boutTags]) => {
            const tag = boutTags[0].tag;
            const tagCount = boutTags.length;

            const userAppliedTag = boutTags.find(
              (boutTag) =>
                typeof username === "string" &&
                boutTag.user?.username === username,
            );

            const handleClick = (event: React.MouseEvent<HTMLDivElement>) => {
              setTagAnchorEls((anchorEls) => ({
                ...anchorEls,
                [tagSlug]: event.currentTarget,
              }));
            };

            const handleClose = () => {
              setTagAnchorEls((anchorEls) => ({
                ...anchorEls,
                [tagSlug]: null,
              }));
            };

            const anchorEl = tagAnchorEls[tagSlug];

            const open = Boolean(anchorEl);
            const id = open ? tagSlug : undefined;

            return (
              <Box key={tagSlug}>
                <Chip
                  aria-describedby={id}
                  onClick={handleClick}
                  variant="outlined"
                  {...(!!userAppliedTag && {
                    onDelete: () =>
                      deleteBoutTag.mutate({ boutTagId: userAppliedTag?.id }),
                  })}
                  color={userAppliedTag ? "primary" : "default"}
                  label={tag?.name}
                  icon={
                    tagCount > 1 ? (
                      <Avatar
                        sx={{
                          width: 24,
                          height: 24,
                          fontSize: 14,
                        }}
                      >
                        {tagCount}
                      </Avatar>
                    ) : (
                      <></>
                    )
                  }
                />
                <Popover
                  id={id}
                  open={open}
                  anchorEl={anchorEl}
                  onClose={handleClose}
                  anchorOrigin={{
                    vertical: "bottom",
                    horizontal: "left",
                  }}
                  sx={{
                    borderRadius: 10,
                    minWidth: (theme) => theme.breakpoints.values.md,
                  }}
                >
                  <Box>
                    <Box p={2} display="flex" flexDirection="column" gap={2}>
                      <Typography variant="body1">{tag?.name}</Typography>
                      {tag?.description && (
                        <Typography variant="body2">
                          {tag?.description}
                        </Typography>
                      )}
                    </Box>
                    <Divider />
                    <Box display="flex" gap={1} p={2}>
                      {boutTags.map((boutTag) => (
                        <Chip
                          key={boutTag.id}
                          label={boutTag.user?.username ?? "(no username)"}
                        />
                      ))}
                    </Box>
                  </Box>
                </Popover>
              </Box>
            );
          })}

          {boutTags.length === 0 && (
            <Typography width={"100%"} textAlign="center">
              No bout tags
            </Typography>
          )}
        </Box>
      </Box>
    </>
  );
}
